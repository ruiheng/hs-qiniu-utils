{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Upload where

import Prelude
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
-- import qualified Data.ByteString.Base64.URL as B64U
-- import qualified Data.ByteString.Char8      as C8
import Control.Applicative                  ((<$>))
import Data.Maybe                           (catMaybes, fromMaybe)
import Data.String                          (fromString)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString.UTF8       as UTF8
import Data.List                            (intersperse, isPrefixOf)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad.Trans.Class            (lift)
import Control.Monad                        (liftM)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Catch                  (MonadThrow, try)
import Data.Monoid                          ((<>))
import Data.Int                             (Int64)
import Control.Monad.Trans.Reader           (ReaderT(..), ask)
import Control.Monad.Logger                 (MonadLogger, logDebugS)

import Network.Wreq
import Control.Lens

-- import Qiniu.Security
import Qiniu.Types
import Qiniu.WS.Types

uploadOneShort ::
    (MonadIO m, MonadThrow m, MonadLogger m) =>
    Maybe ResourceKey
    -> Maybe FilePath   -- ^ optionally reveal the local file path
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> ReaderT String m (WsResult (ByteString, ResourceKey))
uploadOneShort m_key m_fp bs = runExceptT $ do
    upload_token <- lift ask
    let getr = liftIO $ try $ post "http://upload.qiniu.com/" $ catMaybes $
            [ Just $ partText "token" (T.pack upload_token)
            , Just $ partLBS "file" bs & partFileName .~ m_fp
            , (partString "key" . unResourceKey ) <$> m_key
            ]
    rb <- ExceptT getr
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        hash <- liftM fromString $ respJsonGetByKey r "hash"
        key <- liftM ResourceKey $ respJsonGetByKey r "key"
        return (hash, key)


data ChunkPutResult = ChunkPutResult {
                    cprCtx              :: String
                    , cprNextOffset     :: Int64
                    , cprNextHost       :: String
                }
                deriving (Eq, Show)

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


uploadMkblk :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    Int64               -- ^ block size
    -> LB.ByteString    -- ^ content of the file to uploaded
                        -- the first chunk in this block
    -> ReaderT String m (WsResult ChunkPutResult)
uploadMkblk block_size bs = runExceptT $ do
    upload_token <- lift ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                                [ fromString $ "UpToken " ++ upload_token ]
        host = "http://upload.qiniu.com/"
        url  = host ++ "mkblk/" ++ show block_size
    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url bs
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        respJsonGetChunkPutResult r


uploadBput :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    ChunkPutResult       -- ^ previous chunk put result
    -> LB.ByteString
    -> ReaderT String m (WsResult ChunkPutResult)
uploadBput cpr bs = runExceptT $ do
    upload_token <- lift ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                                [ fromString $ "UpToken " ++ upload_token ]
        host = fixHost $ cprNextHost cpr
        offset = cprNextOffset cpr
        ctx = cprCtx cpr
        url = host ++ "/bput/" ++ ctx ++ "/" ++ show offset

    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url bs
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        respJsonGetChunkPutResult r


-- | Upload a whole block: repeatly call uploadBput
uploadOneBlock :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    Int64               -- ^ chunk size
    -> LB.ByteString
    -> ReaderT String m (WsResultP ChunkPutResult)
uploadOneBlock chunk_size bs = runExceptT $ do
    let block_size = LB.length bs
    let (fst_bs, other_bs) = LB.splitAt chunk_size bs
    cpr0 <- ExceptT $ liftM packError $ uploadMkblk block_size fst_bs

    if LB.null other_bs
        then return cpr0
        else do
            let go cpr bs_to_upload = do
                    let (bs1, bs2) = LB.splitAt chunk_size bs_to_upload
                    new_cpr <- ExceptT $ liftM packError $ uploadBput cpr bs1
                    if LB.null bs2
                        then return new_cpr
                        else go new_cpr bs2
            go cpr0 other_bs


uploadMkfile :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    Int64                   -- ^ file size
    -> Maybe ResourceKey
    -> String               -- ^ last host
    -> [String]             -- ^ list of ctx
    -> ReaderT String m (WsResult (ByteString, ResourceKey))
uploadMkfile file_size m_key host ctx_list = runExceptT $ do
    upload_token <- lift ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                                [ fromString $ "UpToken " ++ upload_token ]
        url = fixHost host ++ "/mkfile/" ++ show file_size ++
                (fromMaybe "" $ ("/key/" ++) . unResourceKey <$> m_key)

    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url $
                                    UTF8.fromString $ concat $ intersperse "," ctx_list
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        hash <- liftM fromString $ respJsonGetByKey r "hash"
        key <- liftM ResourceKey $ respJsonGetByKey r "key"
        return (hash, key)


resumableError :: WsResultP a -> Bool
resumableError = const False

uploadByBlocks :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    Int64               -- ^ block size
    -> Int64            -- ^ chunk size
    -> Maybe ResourceKey
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> ReaderT String m (WsResultP (ByteString, ResourceKey))
uploadByBlocks block_size chunk_size m_key bs = runExceptT $ do
    let go cpr_list bs_to_upload = do
            let (bs1, bs2) = LB.splitAt block_size bs_to_upload
            cpr <- ExceptT $ uploadOneBlock chunk_size bs1
            let new_cpr_list = cpr : cpr_list
            if LB.null bs2
                then return new_cpr_list
                else go new_cpr_list bs2

    cprs <- go [] bs
    ExceptT $ liftM packError $ uploadMkfile (LB.length bs) m_key
                                    (cprNextHost $ head cprs)
                                    (reverse $ map cprCtx cprs)

