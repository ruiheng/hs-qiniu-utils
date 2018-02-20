{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Qiniu.Upload where

-- {{{1 imports
import           ClassyPrelude hiding (try, finally)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import           Control.Monad.Catch (try, finally)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM)
import           Control.Concurrent.STM (check)
#if !MIN_VERSION_classy_prelude(1, 0, 0)
import           Control.Concurrent.Async (async)
#endif
import           Control.Concurrent.Async (waitCatch)

import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, object, withObject, (.:), (.:?),
                             (.=))

import           Network.Wreq
import qualified Network.Wreq.Session as WS
import           Control.Lens hiding ((.=))

import           Qiniu.Region
import           Qiniu.Security
import           Qiniu.Types
import           Qiniu.Utils
import           Qiniu.WS.Types
-- }}}1


type QiniuUploadMonad m a = (QiniuRemoteCallMonad m) => ReaderT (Region, UploadToken) m a


uploadOneShot :: Maybe ResourceKey
              -> Maybe ByteString -- ^ optionally specify a mime type
              -> FilePath         -- ^ original file name
              -> LB.ByteString    -- ^ content of the file to uploaded
              -> QiniuUploadMonad m (WsResult UploadedFileInfo)
-- {{{1
uploadOneShot m_key m_mime fp' bs = runExceptT $ do
  sess <- lift $ lift ask
  (region, upload_token) <- ask

  let host = getBaseUrl region ServerUpload False

  -- 七牛要求一定要提供一个文件名，如果没名会出错
  let fp = if null fp'
             then "<unnamed>"
             else fp

  let getr = liftIO $ try $ WS.post sess host $ catMaybes $
        [ Just $ partText "token" (unUploadToken upload_token)
        , Just $ partLBS "file" bs
                 & partFileName .~ (Just fp)
                 & maybe id (\mime -> partContentType .~ Just mime) m_mime
        , (partText "key" . unResourceKey) <$> m_key
        ]
  rb <- ExceptT getr
  asWsResponseNormal' rb
-- }}}1


data ChunkPutResult = ChunkPutResult { cprCtx :: Text, cprOffset :: Int64, cprHost :: Text }
  deriving (Eq, Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = toLower . drop 3 } ''ChunkPutResult)

respJsonGetChunkPutResult :: (MonadThrow m)
                          => Response WsRespBodyNormal
                          -> m ChunkPutResult
-- {{{1
respJsonGetChunkPutResult r = do
  ctx <- respJsonGetByKey r "ctx"
  offset <- respJsonGetByKey r "offset"
  next_host <- respJsonGetByKey r "host"
  return $ ChunkPutResult ctx offset next_host
-- }}}1


fixHost :: (Eq (Element s), Semigroup s, IsString s, IsSequence s) => s -> s
fixHost host = if ("http://" `isPrefixOf` host) || ("https://" `isPrefixOf` host)
                 then host
                 else "http://" <> host


-- | callback with offset of the block
type UploadOpDoneReporter m = Int64 -> ChunkPutResult -> m ()

-- | callback without offset of the block
type UploadOpDoneReporter' m = ChunkPutResult -> m ()

uploadMkblk :: Int64               -- ^ block size
            -> LB.ByteString    -- ^ content of the file to uploaded the first chunk in this block
            -> QiniuUploadMonad m (WsResult ChunkPutResult)
-- {{{1
uploadMkblk block_size bs = runExceptT $ do
  sess <- lift $ lift ask
  (region, upload_token) <- ask
  let opts = defaults & header "Content-Type" .~ ["application/octet-stream"]
             & header "Authorization" .~
             [encodeUtf8 $ "UpToken " <> unUploadToken upload_token]
      host = getBaseUrl region ServerUpload False
      url = host <> "/mkblk/" <> show block_size

  --   $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
  rb <- ExceptT $ liftIO $ try $ WS.postWith opts sess url bs
  runExceptT $ do
    r <- ExceptT $ asWsResponseNormal rb
    respJsonGetChunkPutResult r
-- }}}1

uploadBput :: ChunkPutResult       -- ^ previous chunk put result
           -> LB.ByteString
           -> QiniuUploadMonad m (WsResult ChunkPutResult)
-- {{{1
uploadBput cpr bs = runExceptT $ do
  sess <- lift $ lift ask
  (_region, upload_token) <- ask
  let opts = defaults & header "Content-Type" .~ ["application/octet-stream"]
             & header "Authorization" .~
             [encodeUtf8 $ "UpToken " <> unUploadToken upload_token]
      host = fixHost $ cprHost cpr
      offset = cprOffset cpr
      ctx = cprCtx cpr
      url = unpack $ host <> "/bput/" <> ctx <> "/" <> tshow offset

  --   $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
  (asWsResponseNormal' =<<) $ ExceptT $ liftIO $ try $ WS.postWith opts sess url bs
-- }}}1


-- | Upload a whole block: repeatly call uploadBput
uploadOneBlock :: OnWsCallError m
               -> UploadOpDoneReporter' m
               -> Int64                -- ^ chunk size
               -> LB.ByteString
               -> QiniuUploadMonad m (WsResultP ChunkPutResult)
-- {{{1
uploadOneBlock on_err on_done chunk_size bs = runExceptT $ do
  let block_size = LB.length bs
  let (fst_bs, other_bs) = LB.splitAt chunk_size bs

  cpr0 <- ExceptT $ retryWsCall "uploadMkblk" on_err' $
            liftM packError $ uploadMkblk block_size fst_bs
  lift $ lift $ on_done cpr0

  if LB.null other_bs
    then return cpr0
    else do
      let go cpr bs_to_upload = do
            let (bs1, bs2) = LB.splitAt chunk_size bs_to_upload
            new_cpr <- ExceptT $
                         retryWsCall "uplodBput" on_err' $
                           liftM packError $ uploadBput cpr bs1
            lift $ lift $ on_done new_cpr
            if LB.null bs2
              then return new_cpr
              else go new_cpr bs2
      go cpr0 other_bs

  where
    on_err' x y z = lift $ on_err x y z
-- }}}1


uploadMkfile :: Int64                   -- ^ file size
             -> Maybe FilePath          -- ^ optional: original file name
             -> Maybe ResourceKey
             -> Maybe ByteString     -- ^ optionally specify a mime type
             -> Text               -- ^ last host
             -> [Text]             -- ^ list of ctx
             -> QiniuUploadMonad m (WsResult UploadedFileInfo)
-- {{{1
uploadMkfile file_size m_fp m_key m_mime host ctx_list = runExceptT $ do
  sess <- lift $ lift ask
  (_region, upload_token) <- ask
  let opts = defaults & header "Content-Type" .~ ["application/octet-stream"]
             & header "Authorization" .~
               [encodeUtf8 $ "UpToken " <> unUploadToken upload_token]
      url = unpack $
            fixHost host <>
            "/mkfile/" <>
            tshow file_size <>
            (fromMaybe mempty $ ("/key/" <>) . base64UrlEncodeT <$> maybeNonNull (unResourceKey <$> m_key)) <>
            (fromMaybe mempty $ ("/mimeType/" <>) . base64UrlEncode <$> maybeNonNull m_mime) <>
            (fromMaybe mempty $ ("/fname/" <>) . base64UrlEncodeS <$> maybeNonNull m_fp)
            -- 指定 fname 的这段逻辑，文档上没说，但 js-sdk 就是这样做的

  --   $(logDebugS) logSource $ T.pack $ "POSTing to: " <> url
  rb <- ExceptT $ liftIO $ try $ WS.postWith opts sess url $
          encodeUtf8 $ intercalate "," ctx_list
  asWsResponseNormal' rb
-- }}}1


uploadByBlocks :: forall m. ()
               => OnWsCallError m
               -> UploadOpDoneReporter m
               -> Int64            -- ^ block size
               -> Int64            -- ^ chunk size
               -> Maybe FilePath
               -> Maybe ResourceKey
               -> Maybe ByteString     -- ^ optionally specify a mime type
               -> LB.ByteString    -- ^ content of the file to uploaded
               -> QiniuUploadMonad m (WsResultP UploadedFileInfo)
-- {{{1
uploadByBlocks on_err on_done block_size chunk_size m_fp m_key m_mime bs = runExceptT $ do
  let go cpr_list offset bs_to_upload = do
        let (bs1, bs2) = LB.splitAt block_size bs_to_upload
        cpr <- ExceptT $ uploadOneBlock on_err (on_done offset) chunk_size bs1
        let new_cpr_list = cpr : cpr_list
        if LB.null bs2
          then return new_cpr_list
          else go new_cpr_list (offset + block_size) bs2

  cprs <- go [] 0 bs
  ExceptT $ retryWsCall "uploadMkfile" on_err' $
    liftM packError $ uploadMkfile
                        (LB.length bs)
                        m_fp
                        m_key
                        m_mime
                        (cprHost $
                           fromMaybe (error "cprs should never be empty") $ listToMaybe cprs)
                        (reverse $ map cprCtx cprs)
  where
    on_err' x y z = lift $ on_err x y z
-- }}}1


-- | 断点续传所需的信息
data RecoverUploadInfo =
       RecoverUploadInfo
         { ruiBlockLastCPR :: [Maybe ChunkPutResult]
         -- ^ 每个的 block 的最后一次的上传结果 列表的长度应等于文件分块的数量 Nothing 代表那个 block 从未上传任何 chunk 如果结果内的 offset 等于 block
         -- size 说明上传已完成
         , ruiBlockSize :: Int64
         , ruiChunkSize :: Int64
         , ruiFileName :: Maybe FilePath
         , ruiResourceKey :: Maybe ResourceKey
         , ruiMimeType :: Maybe ByteString
         }

-- {{{1 instances
instance FromJSON RecoverUploadInfo where
  parseJSON = withObject "RecoverUploadInfo" $ \obj ->
    do
      RecoverUploadInfo <$> (obj .: "last-cpr-list")
                        <*> (obj .: "block-size")
                        <*> (obj .: "chunk-size")
                        <*> (obj .:? "file-name")
                        <*> (obj .:? "resource-key")
                        <*> (fmap fromString <$> obj .:? "mime-type")

instance ToJSON RecoverUploadInfo where
  toJSON x = object
               [ "last-cpr-list" .= ruiBlockLastCPR x
               , "block-size" .= ruiBlockSize x
               , "chunk-size" .= ruiChunkSize x
               , "file-name" .= ruiFileName x
               , "resource-key" .= ruiResourceKey x
               , "mime-type" .= fmap C8.unpack (ruiMimeType x)
               ]
-- }}}1


cprMapToRecoverUploadInfo :: Int64
                          -> Int64
                          -> Maybe FilePath
                          -> Maybe ResourceKey
                          -> Maybe ByteString     -- ^ optionally specify a mime type
                          -> Map Int64 ChunkPutResult
                          -> RecoverUploadInfo
-- {{{1
cprMapToRecoverUploadInfo block_size chunk_size m_fp m_key m_mime cpr_map =
  RecoverUploadInfo cpr_list block_size chunk_size m_fp m_key m_mime
  where
    cpr_list = if null cpr_map
                 then []
                 else flip map [0 .. (fst $ Map.findMax cpr_map)] $ \idx -> lookup idx cpr_map
-- }}}1


cprMapFromRecoverUploadInfo :: RecoverUploadInfo
                            -> Map Int64 ChunkPutResult
cprMapFromRecoverUploadInfo rui =
  mapFromList $ catMaybes $
    zipWith (\x y -> fmap (x,) y) [0 ..] $ ruiBlockLastCPR rui


doneBytesLength :: [ChunkPutResult] -> Int64
doneBytesLength lst = foldr ((+) . cprOffset) 0 lst


uploadByBlocksContinue :: forall m. (MonadBaseControl IO m)
                       => OnWsCallError m
                       -> UploadOpDoneReporter m
                       -> Int
                       -> RecoverUploadInfo
                       -> LB.ByteString    -- ^ content of the file to uploaded
                       -> QiniuUploadMonad m (WsResultP UploadedFileInfo)
-- {{{1
uploadByBlocksContinue on_err on_done thread_num0 rui bs = runExceptT $ do
  let block_size = ruiBlockSize rui
      chunk_size = ruiChunkSize rui
      m_fp = ruiFileName rui
      m_key = ruiResourceKey rui
      m_mime = ruiMimeType rui
      expected_blk_num = fromIntegral $ (LB.length bs + block_size - 1) `div` block_size
      blk_cpr_list = take expected_blk_num $ ruiBlockLastCPR rui <> repeat Nothing

  let actions =
        map (ExceptT .
             (uncurry $ uploadOneBlockConinue on_err on_done block_size chunk_size bs)) $ zip [0 ..]
                                                                                            blk_cpr_list

  cprs <- threadPoolRun thread_num actions >>= either throwM return
  ExceptT $ retryWsCall "uploadMkfile" on_err' $
    liftM packError $ uploadMkfile
                        (LB.length bs)
                        m_fp
                        m_key
                        m_mime
                        (cprHost $ fromMaybe (error "cprs should never be empty")
                                     (listToMaybe $ reverse cprs))
                        (map cprCtx cprs)
  where
    thread_num = max 1 thread_num0
    on_err' x y z = lift $ on_err x y z
-- }}}1


threadPoolRun :: (MonadIO m, MonadBaseControl IO m, MonadLogger m)
              => Int
              -> [m a]
              -> m (Either SomeException [a])
-- {{{1
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
                                  run_in_base x `finally` (atomically $ modifyTVar thr_free_tvar (+ 1))
                          _ <- run_in_base $ $(logDebugS) logSource "new thread created"
                          go (aa : rs) xs
                    a_list <- liftM reverse $ go [] actions
                    liftM sequence $ mapM waitCatch a_list
      lift $ mapM restoreM stm_list
-- }}}1


uploadOneBlockConinue :: OnWsCallError m
                      -> UploadOpDoneReporter m
                      -> Int64            -- ^ block size
                      -> Int64            -- ^ chunk size
                      -> LB.ByteString    -- ^ content of the whole file to uploaded
                      -> Int64            -- ^ block index
                      -> Maybe ChunkPutResult
                      -> QiniuUploadMonad m (WsResultP ChunkPutResult)
-- {{{1
uploadOneBlockConinue on_err on_done block_size chunk_size bs idx m_cpr = runExceptT $ do
  let offset = idx * block_size
  let bs_to_upload = LB.take block_size $ LB.drop offset bs
  case m_cpr of
    Nothing -> do
      ExceptT $ uploadOneBlock on_err (on_done offset) chunk_size bs_to_upload
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
                                              chunk_size
                                              block_size
                                              (offset + start_chunk_offset)
                                              []) $ \s -> LB.take chunk_size $ LB.drop s bs

          let bput x y = do
                z <- ExceptT $ retryWsCall "uploadBput" on_err' $
                       liftM packError $ uploadBput x y
                lift $ lift $ on_done offset z
                return z

          foldM bput cpr0 offset_bs_list
  where
    on_err' x y z = lift $ on_err x y z
-- }}}1


-- | 用一个 Chan 实现 UploadOpDoneReporter 需要另一个线程来处理 Chan 里的消息
onDoneWriteChan :: (MonadIO m, MonadLogger m)
                => Chan (Maybe (Int64, ChunkPutResult))
                -> UploadOpDoneReporter m
-- {{{1
onDoneWriteChan done_ch block_offset cpr = do
  $(logDebugS) logSource $
    "block offset " <> tshow block_offset
                       <> " chunk offset " <> tshow (cprOffset cpr)
                                              <> " done."
  liftIO $ writeChan done_ch $ Just (block_offset, cpr)
-- }}}1


onDoneChanWatcher :: MonadIO m
                  => Chan (Maybe (Int64, ChunkPutResult))
                  -> (Int64 -> ChunkPutResult -> m ())
                  -> m ()
-- {{{1
onDoneChanWatcher done_ch handle_msg = do
  let go = do
        m_d <- liftIO $ readChan done_ch
        case m_d of
          Nothing -> return ()
          Just (block_offset, cpr) -> do
            handle_msg block_offset cpr
            go
  go
-- }}}1


-- vim: set foldmethod=marker:
