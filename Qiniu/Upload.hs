{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Qiniu.Upload where

import Prelude
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.TH              as AT
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as C8
#if !MIN_VERSION_base(4, 8, 0)
import Control.Applicative                  ((<$>), (<*>))
#endif
import Data.Maybe                           (catMaybes, fromMaybe)
import Data.String                          (fromString)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString.UTF8       as UTF8
import Data.List                            (intersperse, isPrefixOf)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad.Trans.Class            (lift)
import Control.Monad                        (liftM, foldM)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Catch                  (MonadThrow, try, finally, throwM)
import Data.Monoid                          ((<>))
import Data.Int                             (Int64)
import Data.Char                            (toLower)
import Control.Monad.Reader.Class           (MonadReader, ask)
import Control.Monad.Logger                 (MonadLogger, logDebugS, logInfoS)
import Control.Monad.Trans.Control          (MonadBaseControl, liftBaseWith, restoreM)
import Control.Concurrent.STM.TVar          (newTVarIO, readTVar, modifyTVar, writeTVar)
import Control.Concurrent.STM               (atomically, check)
import Control.Concurrent.Async             (async, waitCatch)
import Control.Concurrent.Chan              (readChan, writeChan, Chan)
import Control.Exception                    (SomeException)
import Data.Map                             (Map)
import qualified Data.Map                   as Map
import Data.Aeson                           (FromJSON, ToJSON, parseJSON, toJSON
                                            , object, withObject, (.:), (.:?), (.=))

import Network.Wreq
import Control.Lens hiding ((.=))

import Qiniu.Security
import Qiniu.Types
import Qiniu.WS.Types


uploadOneShot ::
    (MonadIO m, MonadThrow m, MonadLogger m, MonadReader UploadToken m) =>
    Maybe ResourceKey
    -> Maybe ByteString -- ^ optionally specify a mime type
    -> FilePath         -- ^ original file name
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResult UploadedFileInfo)
uploadOneShot m_key m_mime fp' bs = runExceptT $ do
    upload_token <- ask

    -- 七牛要求一定要提供一个文件名，如果没名会出错
    -- XXX: 但分片上传时没看到哪里需要这个文件名参数
    let fp = if null fp' then "<unnamed>" else fp

    let getr = liftIO $ try $ post "http://upload.qiniu.com/" $ catMaybes $
            [ Just $ partText "token" (fromString $ unUploadToken upload_token)
            , Just $ partLBS "file" bs
                    & partFileName .~ (Just fp)
                    & maybe id (\mime -> partContentType .~ Just mime) m_mime
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
    OnWsCallError m
    -> UploadOpDoneReporter' m
    -> Int64                -- ^ chunk size
    -> LB.ByteString
    -> m (WsResultP ChunkPutResult)
uploadOneBlock on_err on_done chunk_size bs = runExceptT $ do
    let block_size = LB.length bs
    let (fst_bs, other_bs) = LB.splitAt chunk_size bs
    cpr0 <- ExceptT $ retryWsCall "uploadMkblk" on_err $
                        liftM packError $ uploadMkblk block_size fst_bs
    lift $ on_done cpr0

    if LB.null other_bs
        then return cpr0
        else do
            let go cpr bs_to_upload = do
                    let (bs1, bs2) = LB.splitAt chunk_size bs_to_upload
                    new_cpr <- ExceptT $
                                retryWsCall "uplodBput" on_err $
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
    -> Maybe ByteString     -- ^ optionally specify a mime type
    -> String               -- ^ last host
    -> [String]             -- ^ list of ctx
    -> m (WsResult UploadedFileInfo)
uploadMkfile file_size m_key m_mime host ctx_list = runExceptT $ do
    upload_token <- ask
    let opts = defaults & header "Content-Type" .~ [ "application/octet-stream" ]
                        & header "Authorization" .~
                            [ fromString $ "UpToken " ++ unUploadToken upload_token ]
        url = fixHost host ++ "/mkfile/" ++ show file_size
                    ++ (fromMaybe "" $ ("/key/" ++)
                                    . C8.unpack
                                    . B64U.encode
                                    . UTF8.fromString
                                    . unResourceKey
                                    <$> m_key)
                    ++ (fromMaybe "" $ flip fmap m_mime $ ("/mimeType" ++) . C8.unpack . B64U.encode)

    $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
    rb <- ExceptT $ liftIO $ try $ postWith opts url $
                                    UTF8.fromString $ concat $ intersperse "," ctx_list
    asWsResponseNormal' rb


uploadByBlocks :: forall m.
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    OnWsCallError m
    -> UploadOpDoneReporter m
    -> Int64            -- ^ block size
    -> Int64            -- ^ chunk size
    -> Maybe ResourceKey
    -> Maybe ByteString     -- ^ optionally specify a mime type
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResultP UploadedFileInfo)
uploadByBlocks on_err on_done block_size chunk_size m_key m_mime bs = runExceptT $ do
    let go cpr_list offset bs_to_upload = do
            let (bs1, bs2) = LB.splitAt block_size bs_to_upload
            cpr <- ExceptT $ uploadOneBlock
                                on_err (on_done offset) chunk_size bs1
            let new_cpr_list = cpr : cpr_list
            if LB.null bs2
                then return new_cpr_list
                else go new_cpr_list (offset + block_size) bs2

    cprs <- go [] 0 bs
    ExceptT $ retryWsCall "uploadMkfile" on_err $
                liftM packError $ uploadMkfile (LB.length bs) m_key m_mime
                                    (cprHost $ head cprs)
                                    (reverse $ map cprCtx cprs)

-- | 断点续传所需的信息
data RecoverUploadInfo = RecoverUploadInfo {
                        ruiBlockLastCPR         :: [Maybe ChunkPutResult]
                            -- ^ 每个的 block 的最后一次的上传结果
                            -- 列表的长度应等于文件分块的数量
                            -- Nothing 代表那个 block 从未上传任何 chunk
                            -- 如果结果内的 offset 等于 block size 说明上传已完成
                        , ruiBlockSize          :: Int64
                        , ruiChunkSize          :: Int64
                        , ruiResourceKey        :: Maybe ResourceKey
                        , ruiMimeType           :: Maybe ByteString
                        }

instance FromJSON RecoverUploadInfo where
    parseJSON = withObject "RecoverUploadInfo" $ \obj -> do
                RecoverUploadInfo <$> (obj .: "last-cpr-list")
                                <*> (obj .: "block-size")
                                <*> (obj .: "chunk-size")
                                <*> (obj .:? "resource-key")
                                <*> (fmap fromString <$> obj .:? "mime-type")

instance ToJSON RecoverUploadInfo where
    toJSON x = object   [ "last-cpr-list"   .= ruiBlockLastCPR x
                        , "block-size"      .= ruiBlockSize x
                        , "chunk-size"      .= ruiChunkSize x
                        , "resource-key"    .= ruiResourceKey x
                        , "mime-type"       .= fmap C8.unpack (ruiMimeType x)
                        ]

cprMapToRecoverUploadInfo ::
    Int64
    -> Int64
    -> Maybe ResourceKey
    -> Maybe ByteString     -- ^ optionally specify a mime type
    -> Map Int64 ChunkPutResult
    -> RecoverUploadInfo
cprMapToRecoverUploadInfo block_size chunk_size m_key m_mime cpr_map =
    RecoverUploadInfo cpr_list block_size chunk_size m_key m_mime
    where
        cpr_list = if Map.null cpr_map
                    then []
                    else flip map [0..(fst $ Map.findMax cpr_map)] $ \idx ->
                            Map.lookup idx cpr_map

cprMapFromRecoverUploadInfo ::
    RecoverUploadInfo
    -> Map Int64 ChunkPutResult
cprMapFromRecoverUploadInfo rui =
    Map.fromList $ catMaybes $
        zipWith (\x y -> fmap (x,) y) [0..] $ ruiBlockLastCPR rui

doneBytesLength :: [ChunkPutResult] -> Int64
doneBytesLength lst = foldr ((+) . cprOffset) 0 lst


uploadByBlocksContinue :: forall m.
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadBaseControl IO m
    , MonadReader UploadToken m
    ) =>
    OnWsCallError m
    -> UploadOpDoneReporter m
    -> Int
    -> RecoverUploadInfo
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResultP UploadedFileInfo)
uploadByBlocksContinue on_err on_done thread_num rui bs = runExceptT $ do
    let block_size  = ruiBlockSize rui
        chunk_size  = ruiChunkSize rui
        m_key       = ruiResourceKey rui
        m_mime      = ruiMimeType rui
        expected_blk_num = fromIntegral $ (LB.length bs + block_size - 1) `div` block_size
        blk_cpr_list = take expected_blk_num $ ruiBlockLastCPR rui ++ repeat Nothing

    let actions =
            map (ExceptT .
                    (uncurry $ uploadOneBlockConinue
                                on_err on_done block_size chunk_size bs)
                    )
                    $ zip [0..] blk_cpr_list

    cprs <- threadPoolRun thread_num actions >>= either throwM return
    ExceptT $ retryWsCall "uploadMkfile" on_err $
                liftM packError $ uploadMkfile (LB.length bs) m_key m_mime
                                    (cprHost $ last cprs)
                                    (map cprCtx cprs)

threadPoolRun :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) =>
    Int
    -> [m a]
    -> m (Either SomeException [a])
threadPoolRun thread_num actions = runExceptT $ do
    if thread_num <= 1
        then lift $ sequence actions
        else do
            stm_list <- ExceptT $ liftBaseWith $ \run_in_base -> do
                thr_free_tvar <- newTVarIO thread_num
                let go rs [] = return rs
                    go rs (x:xs) = do
                        atomically $ do
                            thr_free <- readTVar thr_free_tvar
                            check $ thr_free > 0
                            writeTVar thr_free_tvar (thr_free - 1)
                        aa <- async $ do
                                run_in_base x `finally`
                                    ( atomically $ modifyTVar thr_free_tvar (+1) )
                        _ <- run_in_base $ $(logDebugS) logSource "new thread created"
                        go (aa:rs) xs
                a_list <- liftM reverse $ go [] actions
                liftM sequence $ mapM waitCatch a_list
            lift $ mapM restoreM stm_list


uploadOneBlockConinue ::
    ( MonadIO m, MonadThrow m, MonadLogger m
    , MonadReader UploadToken m
    ) =>
    OnWsCallError m
    -> UploadOpDoneReporter m
    -> Int64            -- ^ block size
    -> Int64            -- ^ chunk size
    -> LB.ByteString    -- ^ content of the whole file to uploaded
    -> Int64            -- ^ block index
    -> Maybe ChunkPutResult
    -> m (WsResultP ChunkPutResult)
uploadOneBlockConinue on_err on_done block_size chunk_size bs idx m_cpr = runExceptT $ do
        let offset          = idx * block_size
        let bs_to_upload    = LB.take block_size $ LB.drop offset bs
        case m_cpr of
            Nothing -> do
                ExceptT $ uploadOneBlock
                            on_err (on_done offset) chunk_size bs_to_upload
            Just cpr0 -> do
                let start_chunk_offset = cprOffset cpr0
                if start_chunk_offset >= block_size
                    then return cpr0
                    else do
                        let mk_intv step limit x rs =
                                if x < limit
                                    then mk_intv step limit (x + step) (x : rs)
                                    else rs
                        let offset_bs_list = flip map
                                        (reverse $ mk_intv
                                                chunk_size block_size
                                                (offset + start_chunk_offset) [])
                                        $ \s -> LB.take chunk_size $ LB.drop s bs

                        let bput x y = do
                                    z <- ExceptT $ retryWsCall "uploadBput" on_err $
                                            liftM packError $ uploadBput x y
                                    lift $ on_done offset z
                                    return z

                        foldM bput cpr0 offset_bs_list


-- | 用一个 Chan 实现 UploadOpDoneReporter
-- 需要另一个线程来处理 Chan 里的消息
onDoneWriteChan :: (MonadIO m, MonadLogger m) =>
    Chan (Maybe (Int64, ChunkPutResult))
    -> UploadOpDoneReporter m
onDoneWriteChan done_ch block_offset cpr = do
    $(logInfoS) logSource $ fromString $
        "block offset " ++ show block_offset
            ++ " chunk offset " ++ show (cprOffset cpr)
            ++ " done."
    liftIO $ writeChan done_ch $ Just (block_offset, cpr)


onDoneChanWatcher :: MonadIO m =>
    Chan (Maybe (Int64, ChunkPutResult))
    -> (Int64 -> ChunkPutResult -> m ())
    -> m ()
onDoneChanWatcher done_ch handle_msg = do
    let go = do
            m_d <- liftIO $ readChan done_ch
            case m_d of
                Nothing                     -> return ()
                Just (block_offset, cpr)    -> do
                    handle_msg block_offset cpr
                    go
    go
