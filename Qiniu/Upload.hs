{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Qiniu.Upload where

import Prelude
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.TH              as AT
-- import qualified Data.ByteString.Base64.URL as B64U
-- import qualified Data.ByteString.Char8      as C8
import Control.Applicative                  ((<$>))
import Data.Maybe                           (catMaybes, fromMaybe)
import Data.String                          (fromString)
-- import Data.ByteString                      (ByteString)
import qualified Data.ByteString.UTF8       as UTF8
import Data.List                            (intersperse, isPrefixOf)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad.Trans.Class            (lift)
import Control.Monad                        (liftM)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Catch                  (MonadThrow, try)
import Data.Monoid                          ((<>))
import Data.Int                             (Int64)
import Data.Char                            (toLower)
import Control.Monad.Reader.Class           (MonadReader, ask)
import Control.Monad.Logger                 (MonadLogger, logDebugS)

import Network.Wreq
import Control.Lens

import Qiniu.Security
import Qiniu.Types
import Qiniu.WS.Types

data UploadedFileInfo = UploadedFileInfo {
                            ufiHash     :: String
                            , ufiKey    :: ResourceKey
                        }
                        deriving (Eq, Show)

$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = map toLower . drop 3}
    ''UploadedFileInfo)

uploadOneShort ::
    (MonadIO m, MonadThrow m, MonadLogger m, MonadReader UploadToken m) =>
    Maybe ResourceKey
    -> Maybe FilePath   -- ^ optionally reveal the local file path
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResult UploadedFileInfo)
uploadOneShort m_key m_fp bs = runExceptT $ do
    upload_token <- ask
    let getr = liftIO $ try $ post "http://upload.qiniu.com/" $ catMaybes $
            [ Just $ partText "token" (fromString $ unUploadToken upload_token)
            , Just $ partLBS "file" bs & partFileName .~ m_fp
            , (partString "key" . unResourceKey ) <$> m_key
            ]
    rb <- ExceptT getr
    asWsResponseNormal' rb


data ChunkPutResult = ChunkPutResult {
                    cprCtx          :: String
                    , cprOffset     :: Int64
                    , cprHost       :: String
                }
                deriving (Eq, Show)

$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = map toLower . drop 3}
    ''ChunkPutResult)

respJsonGetChunkPutResult :: (MonadThrow m) =>
    Response WsRespBodyNormal
    -> m ChunkPutResult
respJsonGetChunkPutResult r = do
    ctx <- respJsonGetByKey r "ctx"
    offset <- respJsonGetByKey r "offset"
    next_host <- respJsonGetByKey r "host"
    return $ ChunkPutResult ctx offset next_host


fixHost :: String -> String
fixHost host = if ("http://" `isPrefixOf` host) || ("https://" `isPrefixOf` host)
                then host
                else "http://" ++ host


-- | callback with offset of the block
type UploadOpDoneReporter m = Int64 -> ChunkPutResult -> m ()

-- | callback without offset of the block
type UploadOpDoneReporter' m = ChunkPutResult -> m ()


uploadMkblk :: (MonadIO m, MonadThrow m, MonadLogger m, MonadReader UploadToken m) =>
    Int64               -- ^ block size
    -> LB.ByteString    -- ^ content of the file to uploaded
                        -- the first chunk in this block
    -> m (WsResult ChunkPutResult)
uploadMkblk block_size bs = runExceptT $ do
    upload_token <- ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                            [ fromString $ "UpToken " ++ unUploadToken upload_token ]
        host = "http://upload.qiniu.com/"
        url  = host ++ "mkblk/" ++ show block_size
    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url bs
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        respJsonGetChunkPutResult r


uploadBput ::
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    ChunkPutResult       -- ^ previous chunk put result
    -> LB.ByteString
    -> m (WsResult ChunkPutResult)
uploadBput cpr bs = runExceptT $ do
    upload_token <- ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                            [ fromString $ "UpToken " ++ unUploadToken upload_token ]
        host = fixHost $ cprHost cpr
        offset = cprOffset cpr
        ctx = cprCtx cpr
        url = host ++ "/bput/" ++ ctx ++ "/" ++ show offset

    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    (asWsResponseNormal' =<<) $ ExceptT $ liftIO $ try $ postWith opts url bs


-- | Upload a whole block: repeatly call uploadBput
uploadOneBlock ::
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    ErrorReporter m
    -> UploadOpDoneReporter' m
    -> Int64                -- ^ chunk size
    -> LB.ByteString
    -> m (WsResultP ChunkPutResult)
uploadOneBlock on_err on_done chunk_size bs = runExceptT $ do
    let block_size = LB.length bs
    let (fst_bs, other_bs) = LB.splitAt chunk_size bs
    cpr0 <- ExceptT $ retryWsCall'' "uploadMkblk" on_err $
                        liftM packError $ uploadMkblk block_size fst_bs
    lift $ on_done cpr0

    if LB.null other_bs
        then return cpr0
        else do
            let go cpr bs_to_upload = do
                    let (bs1, bs2) = LB.splitAt chunk_size bs_to_upload
                    new_cpr <- ExceptT $
                                retryWsCall'' "uplodBput" on_err $
                                        liftM packError $ uploadBput cpr bs1
                    lift $ on_done new_cpr
                    if LB.null bs2
                        then return new_cpr
                        else go new_cpr bs2
            go cpr0 other_bs


uploadMkfile ::
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    Int64                   -- ^ file size
    -> Maybe ResourceKey
    -> String               -- ^ last host
    -> [String]             -- ^ list of ctx
    -> m (WsResult UploadedFileInfo)
uploadMkfile file_size m_key host ctx_list = runExceptT $ do
    upload_token <- ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                            [ fromString $ "UpToken " ++ unUploadToken upload_token ]
        url = fixHost host ++ "/mkfile/" ++ show file_size ++
                (fromMaybe "" $ ("/key/" ++) . unResourceKey <$> m_key)

    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url $
                                    UTF8.fromString $ concat $ intersperse "," ctx_list
    asWsResponseNormal' rb


uploadByBlocks :: forall m.
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    ErrorReporter m
    -> UploadOpDoneReporter m
    -> Int64            -- ^ block size
    -> Int64            -- ^ chunk size
    -> Maybe ResourceKey
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResultP UploadedFileInfo)
uploadByBlocks on_err on_done block_size chunk_size m_key bs = runExceptT $ do
    let go cpr_list offset bs_to_upload = do
            let (bs1, bs2) = LB.splitAt block_size bs_to_upload
            cpr <- ExceptT $ uploadOneBlock
                                on_err (on_done offset) chunk_size bs1
            let new_cpr_list = cpr : cpr_list
            if LB.null bs2
                then return new_cpr_list
                else go new_cpr_list (offset + block_size) bs2

    cprs <- go [] 0 bs
    ExceptT $ retryWsCall'' "uploadMkfile" on_err $
                liftM packError $ uploadMkfile (LB.length bs) m_key
                                    (cprHost $ head cprs)
                                    (reverse $ map cprCtx cprs)

