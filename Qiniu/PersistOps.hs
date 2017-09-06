module Qiniu.PersistOps
  ( PersistentId(..)
  , QiniuPfopMonad
  , AvthumbFormat
  , AvthumbSubOp(..)
  , AvthumbOp(..)
  , PersistOpStatus(..)
  , persistOpStatusFromCode
  , persistOpStatusToCode
  , persistOpStatusSucceeded
  , persistOpStatusInProgress
  , PersistOpInfo(..)
  , PfopInfoItem(..)
  , persistOpsOnSaved
  , persistOpsQuery
  ) where

-- {{{1 imports
import ClassyPrelude hiding (try)
import Control.Monad.Catch                  (try)
import Control.Monad.Logger
import Control.Monad.Except                 (runExceptT, ExceptT(..))
import Data.Aeson
#if defined(PERSISTENT)
import Database.Persist                     (PersistField)
import Database.Persist.Sql                 (PersistFieldSql)
#endif
import Network.HTTP.Client                  ( httpLbs, Request, Manager, host, path
                                            , urlEncodedBody, setQueryString
                                            , defaultRequest
                                            )

import Qiniu.Types
import Qiniu.Security
import Qiniu.WS.Types
-- }}}1



-- | 持久化接口的接口服务器
persistOpApiHost :: IsString a => a
persistOpApiHost = "api.qiniu.com"


persistOpApiReqGet :: ByteString -> Request
persistOpApiReqGet uri_path =
  defaultRequest { host = persistOpApiHost, path = uri_path }


persistOpApiReqPost :: [(ByteString, ByteString)] -> ByteString -> Request
persistOpApiReqPost post_params uri_path =
  urlEncodedBody post_params $ persistOpApiReqGet uri_path


newtype PersistentId = PersistentId { unPersistentId :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)
#if defined(PERSISTENT)
deriving instance PersistField PersistentId

deriving instance PersistFieldSql PersistentId
#endif
type QiniuPfopMonad m = (MonadIO m, MonadCatch m, MonadLogger m, MonadReader Manager m)

-- | 音视频处理的格式参数
type AvthumbFormat = Text

-- | 音视频处理
data AvthumbSubOp = AvthumbOpAudioBitRate Int        -- ^ /ab/<BitRate>k
                  | AvthumbOpAudioQuality Int   -- ^ /aq/<AudioQuality>
                  | AvthumbOpSamplingRate Int   -- ^ 采样频率，单位HZ
                  | AvthumbOpFrameRate Int      -- ^ 视频帧率
                  | AvthumbOpVideoBitRate Int   -- ^ 视频码率 kHz
                  | AvthumbOpVideoCodec Text    -- ^ 视频编码格式
                  | AvthumbOpAudioCodec Text    -- ^ 编码方案．如 libx264
                  | AvthumbOpSubtitleCodec Text -- ^ 字幕编码方案
                  | AvthumbOpVideoResolution (Either (Int, Int) Text)  -- ^ 视频分辨率
                  | AvthumbOpVideoAutoscale Bool -- ^ 视频是否按原比例缩放
                  | AvthumbOpStripMeta Bool     -- ^ 是否去除 meta 信息
                  | AvthumbOpNoSubtitle Bool    -- ^ 是否去除字幕
  deriving (Show, Eq, Ord)

encodeAvthumbOpAsPath :: AvthumbSubOp -> Text
-- {{{1
encodeAvthumbOpAsPath (AvthumbOpAudioBitRate k)               = "ab/" <> tshow k <> "k"
encodeAvthumbOpAsPath (AvthumbOpAudioQuality q)               = "aq/" <> tshow q
encodeAvthumbOpAsPath (AvthumbOpSamplingRate r)               = "ar/" <> tshow r
encodeAvthumbOpAsPath (AvthumbOpAudioCodec c)                 = "acodec/" <> c
encodeAvthumbOpAsPath (AvthumbOpFrameRate r)                  = "r/" <> tshow r
encodeAvthumbOpAsPath (AvthumbOpVideoBitRate r)               = "vb/" <> tshow r <> "k"
encodeAvthumbOpAsPath (AvthumbOpVideoCodec t)                 = "vcodec/" <> t
encodeAvthumbOpAsPath (AvthumbOpSubtitleCodec t)              = "scodec/" <> t
encodeAvthumbOpAsPath (AvthumbOpVideoResolution (Left (x,y))) = "s/" <> tshow x <> "x" <> tshow y
encodeAvthumbOpAsPath (AvthumbOpVideoResolution (Right n))    = "s/" <> n
encodeAvthumbOpAsPath (AvthumbOpVideoAutoscale b)             = "autoscale/" <> if b then "1" else "0"
encodeAvthumbOpAsPath (AvthumbOpStripMeta b)                  = "stripmeta/" <> if b then "1" else "0"
encodeAvthumbOpAsPath (AvthumbOpNoSubtitle b)                 = "sn/" <> if b then "1" else "0"
-- }}}1


-- | 音视频处理完整指令
data AvthumbOp = AvthumbOp AvthumbFormat [AvthumbSubOp]

instance PersistFop AvthumbOp where
  encodeFopToText (AvthumbOp format sub_ops) =
    mconcat $ intersperse "/" $ ("avthumb/" <> format) : map encodeAvthumbOpAsPath (sort sub_ops)

data PfopResp = PfopResp { unPfopResp :: PersistentId }

instance FromJSON PfopResp where
  parseJSON = withObject "PfopResp" $ \o -> PfopResp <$> o .: "persistentId"

-- | 对已有的资源执行持久化数据处理
persistOpsOnSaved :: QiniuPfopMonad m
                  => SecretKey
                  -> AccessKey
                  -> [FopCmd]
                  -> Entry          -- ^ input resource
                  -> Maybe Text     -- ^ notify url
                  -> Maybe Pipeline -- ^ pipeline
                  -> Bool
                  -> m (WsResult PersistentId)
-- {{{1
persistOpsOnSaved secret_key access_key ops (bucket, rkey) m_notify_url m_pipeline forced = runExceptT $ do
  mgmt <- ask
  req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
  fmap (fmap unPfopResp) $ (asWsResponseNormal' =<<) $
    ExceptT $ try $ liftIO $ httpLbs req' mgmt
  where
    url_path = "/pfop/"
    fops_cmd = encodeFopCmdList ops :: Text
    post_data = catMaybes
                  [ Just $ ("bucket", encodeUtf8 (fromString $ unBucket bucket))
                  , Just $ ("key", encodeUtf8 (fromString $ unResourceKey rkey))
                  , Just $ ("fops", (encodeUtf8 fops_cmd :: ByteString))
                  , flip fmap m_notify_url $ \x -> ("notifyURL", encodeUtf8 x)
                  , flip fmap m_pipeline $ \pl -> ("pipeline", encodeUtf8 (unPipeline pl))
                  , if forced
                      then Just $ ("force", "1")
                      else Nothing
                  ]
    req = persistOpApiReqPost post_data url_path
-- }}}1


-- | 持久化处理结果状态码
data PersistOpStatus = PersistOpSucceeded
                     | PersistOpPending
                     | PersistOpProcessing
                     | PersistOpFailed
                     | PersistOpNotifyFailed
                     | PersistOpStatusOther Int
  deriving (Show, Eq)

-- {{{1 instances
instance FromJSON PersistOpStatus where
  parseJSON v = persistOpStatusFromCode <$> parseJSON v
-- }}}1

persistOpStatusFromCode :: Int -> PersistOpStatus
-- {{{1
persistOpStatusFromCode code =
  case code of
    0 -> PersistOpSucceeded
    1 -> PersistOpPending
    2 -> PersistOpProcessing
    3 -> PersistOpFailed
    4 -> PersistOpNotifyFailed
    _ -> PersistOpStatusOther code
-- }}}1

persistOpStatusToCode :: PersistOpStatus -> Int
-- {{{1
persistOpStatusToCode PersistOpSucceeded       = 0
persistOpStatusToCode PersistOpPending         = 1
persistOpStatusToCode PersistOpProcessing      = 2
persistOpStatusToCode PersistOpFailed          = 3
persistOpStatusToCode PersistOpNotifyFailed    = 4
persistOpStatusToCode (PersistOpStatusOther c) = c
-- }}}1


-- | PersistOpNotifyFailed 的意义不明，目前认为这为是处理本身是成功的 只是通知发生错误而已
persistOpStatusSucceeded :: PersistOpStatus -> Bool
-- {{{1
persistOpStatusSucceeded PersistOpSucceeded = True
persistOpStatusSucceeded PersistOpNotifyFailed = True
persistOpStatusSucceeded _ = False
-- }}}1


persistOpStatusInProgress :: PersistOpStatus -> Bool
-- {{{1
persistOpStatusInProgress PersistOpPending    = True
persistOpStatusInProgress PersistOpProcessing = True
persistOpStatusInProgress _                   = False
-- }}}1


data PersistOpInfo =
       PersistOpInfo
         { pfopInfoId :: PersistentId
         , pfopInfoStatus :: PersistOpStatus
         , pfopInfoStatusDesc :: Text
         , pfopInfoInputKey :: ResourceKey
         , pfopInfoInputBucket :: Bucket
         , pfopInfoItems :: [PfopInfoItem]
         , pfopInfoPipeline :: Pipeline
         , pfopInfoReqid :: Text
         }
  deriving (Show)

-- {{{1 instances
instance FromJSON PersistOpInfo where
  parseJSON = withObject "PersistOpInfo" $ \o ->
    do
      PersistOpInfo <$> o .: "id"
                    <*> o .: "code"
                    <*> o .: "desc"
                    <*> o .: "inputKey"
                    <*> (Bucket <$> o .: "inputBucket")
                    <*> o .: "items"
                    <*> o .: "pipeline"
                    <*> o .: "reqid"
-- }}}1


data PfopInfoItem =
       PfopInfoItem
         { pfopInfoItemCmd :: Text
         , pfopInfoItemStatus :: PersistOpStatus
         , pfopInfoItemStatusDesc :: Text
         , pfopInfoItemError :: Text
         , pfopInfoItemHash :: ByteString
         , pfopInfoItemKey :: ResourceKey
         , pfopInfoItemReturnOld :: Bool
         }
  deriving (Show, Eq)

-- {{{1 instances
instance FromJSON PfopInfoItem where
  parseJSON = withObject "PfopInfoItem" $ \o ->
    do
      PfopInfoItem <$> o .: "cmd"
                   <*> o .: "code"
                   <*> o .: "desc"
                   <*> o .: "error"
                   <*> fmap fromString (o .: "hash")
                   <*> o .: "key"
                   <*> (fmap (/= (0 :: Int)) $ o .: "returnOld")
-- }}}1

-- | 持久化处理状态查询
persistOpsQuery :: QiniuPfopMonad m
                => SecretKey
                -> AccessKey
                -> PersistentId
                -> m (WsResult PersistOpInfo)
-- {{{1
persistOpsQuery secret_key access_key pid = runExceptT $ do
  mgmt <- ask
  req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
  (asWsResponseNormal' =<<) $
    ExceptT $ try $ liftIO $ httpLbs req' mgmt
  where
    url_path = "/status/get/prefop"
    req = setQueryString [("id", Just (encodeUtf8 $ unPersistentId pid))] $ persistOpApiReqPost []
                                                                              url_path
-- }}}1



-- vim: set foldmethod=marker:
