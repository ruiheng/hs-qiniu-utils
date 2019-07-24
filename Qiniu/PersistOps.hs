module Qiniu.PersistOps
  ( PersistentId(..)
  , QiniuPfopMonad
  , ImageView2Mode(..)
  , ImageView2Dim(..)
  , ImageView2(..)
  , AvthumbFormat
  , AvthumbSubOp(..)
  , AvthumbOp(..)
  , Rotation(..)
  , VFrameOp(..)
  , VSampleOp(..)
  , AvInfoAudio(..)
  , AvInfoFormat(..)
  , AvInfoVideo(..)
  , AvInfo(..)
  , ImageInfo(..)
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
import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Except (runExceptT, ExceptT(..))
import           Data.Aeson
import           Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import           Data.Aeson.Types (camelTo2, typeMismatch)
import qualified Data.ByteString.Char8 as C8
#if defined(PERSISTENT)
import           Database.Persist (PersistField)
import           Database.Persist.Sql (PersistFieldSql)
#endif

import           Network.Wreq               (defaults, param, FormParam((:=)))
import qualified Network.Wreq.Session       as WS

import Qiniu.Types
import Qiniu.Security
import Qiniu.WS.Types
import Qiniu.Utils
-- }}}1



-- | 持久化接口的接口服务器
persistOpApiHost :: IsString a => a
persistOpApiHost = "api.qiniu.com"

persistOpApiUrl :: String -> String
persistOpApiUrl p = "http://" <> persistOpApiHost <> p


newtype PersistentId = PersistentId { unPersistentId :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)
#if defined(PERSISTENT)
deriving instance PersistField PersistentId

deriving instance PersistFieldSql PersistentId
#endif

type QiniuPfopMonad m a = (QiniuRemoteCallMonad m) => ReaderT (SecretKey, AccessKey) m a


data ImageView2Mode = ImageView2Mode0MaxEdge
                    | ImageView2Mode1MinSize
                    | ImageView2Mode2MaxSize
                    | ImageView2Mode3MinSize
                    | ImageView2Mode4MaxEdge
                    | ImageView2Mode5MminEdge
                    deriving (Show, Eq, Enum, Bounded)

-- | XXX:  注意此函数实现依赖上面定义时的ctor顺序
encodeImageView2Mode :: ImageView2Mode -> Text
encodeImageView2Mode = ("/" <>) . tshow . fromEnum


data ImageView2Dim = ImageView2DimX Int
                   | ImageView2DimY Int
                   | ImageView2DimXY Int Int

encodeImageView2Dim :: ImageView2Dim -> Text
encodeImageView2Dim (ImageView2DimX x)    = "/w/" <> tshow x
encodeImageView2Dim (ImageView2DimY y)    = "/h/" <> tshow y
encodeImageView2Dim (ImageView2DimXY x y) = "/w/" <> tshow x <> "/h/" <> tshow y


-- | 图片基本处理
-- https://developer.qiniu.com/dora/manual/1279/basic-processing-images-imageview2
data ImageView2 = ImageView2 ImageView2Mode ImageView2Dim
                    (Maybe Text)  -- format
                    (Maybe Bool)    -- 是否渐进
                    (Maybe Int)    -- quality
                    (Maybe Bool)    -- ignore error


instance PersistFop ImageView2 where
-- {{{1
  encodeFopToText (ImageView2 mode dim m_format m_interlace m_quality m_ignore_error) =
    mconcat $ catMaybes $
      [ Just $ "imageView2" <> encodeImageView2Mode mode <> encodeImageView2Dim dim
      , ("/format/" <>) <$> m_format
      , ("/interlace/" <>) . encode_bool <$> m_interlace
      , ("/q/" <>) . tshow <$> m_quality
      , ("/ignore-error/" <>) . encode_bool <$> m_ignore_error
      ]
    where
      encode_bool True = "1"
      encode_bool False = "0"
-- }}}1


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


-- | 旋转角度的选择
data Rotation = RotateClockwiseQuarter Int  -- ^ 顺时针转多少个象限.
              | RotateAuto
              deriving (Show)


encRotation :: Rotation -> Text
encRotation (RotateClockwiseQuarter q)  = if q < 0
                                            then encRotation RotateAuto
                                            else case (q `rem` 4) of
                                                   qq | qq == 0 -> encRotation RotateAuto
                                                      | otherwise -> tshow (90 * qq)
encRotation RotateAuto                  = "auto"


-- | 视频帧缩略图
data VFrameOp = VFrameOp Text Float (Maybe (Int, Int)) (Maybe Rotation)

instance PersistFop VFrameOp where
-- {{{1
  encodeFopToText (VFrameOp fmt offset m_size m_rotate) =
    mconcat $ catMaybes
      [ Just $ "vframe/" <> fmt
      , Just $ "/offset/" <> tshow offset
      , flip fmap m_size $ \ (w, h) -> "/w/" <> tshow w <> "/h/" <> tshow h
      , flip fmap m_rotate $ \ r -> "/rotate/" <> encRotation r
      ]
    where
-- }}}1


-- | 视频采样缩略图
data VSampleOp = VSampleOp
                  Text    -- format
                  Text    -- pattern
                  Float   -- start time
                  Float   -- duration
                  (Maybe (Int, Int))
                  (Maybe Rotation)
                  (Maybe Float)

instance PersistFop VSampleOp where
-- {{{1
  encodeFopToText (VSampleOp format pattern start_time duration m_size m_rotate m_interval) =
    mconcat $ catMaybes
      [ Just $ "vsample/" <> format
      , Just $ "/ss/" <> tshow start_time
      , Just $ "/t/" <> tshow duration
      , flip fmap m_size $ \ (w, h) -> "/s/" <> tshow w <> "x" <> tshow h
      , flip fmap m_rotate $ \ r -> "/rotate/" <> encRotation r
      , flip fmap m_interval $ \ iv -> "/interval/" <> tshow iv
      , Just $ "/pattern/" <> base64UrlEncodeT pattern
      ]
-- }}}1


-- | 暂时不实用．见 AvInfo 注释
data AvInfoAudio =
  AvInfoAudio
    { avInfoAudioBitRate    :: StrNumber Int
    , avInfoAudioChannels   :: Int
    , avInfoAudioCodecName  :: Text
    , avInfoAudioCodecType  :: Text
    , avInfoAudioDuration   :: StrNumber Float
    , avInfoAudioIndex      :: Int
    , avInfoAudioNbFrames   :: StrNumber Int
    , avInfoAudioRFrameRate :: Text
    , avInfoAudioSampleFmt  :: Text
    , avInfoAudioSampleRate :: StrNumber Int
    , avInfoAudioStartTime  :: StrNumber Float
    , avInfoAudioTags       :: Map Text Text
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }) ''AvInfoAudio)

-- | 暂时不实用．见 AvInfo 注释
data AvInfoFormat =
  AvInfoFormat
    { avInfoFormatBitRate        :: StrNumber Int
    , avInfoFormatDuration       :: StrNumber Float
    , avInfoFormatFormatLongName :: Text
    , avInfoFormatFormatName     :: Text
    , avInfoFormatNbStreams      :: StrNumber Int
    , avInfoFormatSize           :: StrNumber Int
    , avInfoFormatStartTime      :: StrNumber Float
    , avInfoFormatTags           :: Map Text Text
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }) ''AvInfoFormat)

-- | 暂时不实用．见 AvInfo 注释
data AvInfoVideo =
  AvInfoVideo
    { avInfoVideoBitRate            :: StrNumber Int
    , avInfoVideoCodecName          :: Text
    , avInfoVideoCodecType          :: Text
    , avInfoVideoDisplayAspectRatio :: Text
    , avInfoVideoDuration           :: StrNumber Float
    , avInfoVideoHeight             :: Int
    , avInfoVideoWidth              :: Int
    , avInfoVideoIndex              :: Int
    , avInfoVideoNbFrames           :: StrNumber Int
    , avInfoVideoRFrameRate         :: Text
    , avInfoVideoSampleAspectRatio  :: Text
    , avInfoVideoStartTime          :: StrNumber Float
    , avInfoVideoTags               :: Map Text Text
    }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }) ''AvInfoVideo)


-- | 魔法变量中的 avinfo 字段结构．非 avinfo 接口报文
-- XXX: 实测表明，若魔法变量指定例如 ${avinfo} 这样的格式，七牛回调时推送的json报文变成非法json
--      实用上，目前只能具体取所要的特定字段. 即例如 $(imageInfo.height)
data AvInfo = AvInfo { avInfoAudio  :: AvInfoAudio
                     , avInfoVideo  :: AvInfoVideo
                     , avInfoFormat :: AvInfoFormat
                     }
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }) ''AvInfo)

instance FromJSON (MaybeMagic AvInfo) where
  parseJSON Null = return $ MaybeMagic Nothing

  -- 核心逻辑就是检测 format 字典下 duration 至少应该有值
  parseJSON v@(Object o) = fmap MaybeMagic $ do
    m_fmt_o <- o .:? "format"
    case m_fmt_o of
      Nothing -> return Nothing
      Just fmt_o -> do
        has_duration <- fmap nonNullStrNumber $ fmt_o .: "duration"
        if has_duration
           then fmap Just $ parseJSON v
           else return Nothing

  parseJSON x = typeMismatch "AvInfo object" x


-- | 暂时不实用．见 AvInfo 注释
data ImageInfo = ImageInfo { imageInfoFormat     :: Text
                           , imageInfoWidth      :: Int
                           , imageInfoHeight     :: Int
                           , imageInfoColorModel :: Text
                           }
$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 9 }) ''ImageInfo)


data PfopResp = PfopResp { unPfopResp :: PersistentId }

instance FromJSON PfopResp where
  parseJSON = withObject "PfopResp" $ \o -> PfopResp <$> o .: "persistentId"


-- | 对已有的资源执行持久化数据处理
persistOpsOnSaved :: [FopCmd]
                  -> Entry          -- ^ input resource
                  -> Maybe Text     -- ^ notify url
                  -> Maybe Pipeline -- ^ pipeline
                  -> Bool
                  -> QiniuPfopMonad m (WsResult PersistentId)
-- {{{1
persistOpsOnSaved ops (bucket, rkey) m_notify_url m_pipeline forced = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path (Just post_data) defaults
  fmap (fmap unPfopResp) $ (asWsResponseNormal' =<<) $
    ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data
  where
    url_path = "/pfop/"
    fops_cmd = encodeFopCmdList ops :: Text
    post_data = catMaybes
                  [ Just $ "bucket" := unBucket bucket
                  , Just $ "key" := unResourceKey rkey
                  , Just $ "fops" := fops_cmd
                  , flip fmap m_notify_url $ \ x -> "notifyURL" := x
                  , flip fmap m_pipeline $ \ pl -> "pipeline" := unPipeline pl
                  , if forced
                       then Just $ "force" := ("1" :: String)
                       else Nothing
                  ]
    url = persistOpApiUrl $ C8.unpack url_path
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


-- | XXX: 按文档，回调时从七牛发出的报文，与主动查询时回复的报文有点不一样
-- 主动查询回复报文内容更多
-- 暂时理解为回调报文文档未更新同步，它应该与主动查询时得到相同的内容
-- 实测已证实以上推测是对的
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
                    <*> o .: "inputBucket"
                    <*> o .: "items"
                    <*> o .: "pipeline"
                    <*> o .: "reqid"
-- }}}1


data PfopInfoItem =
       PfopInfoItem
         { pfopInfoItemCmd :: Text
         , pfopInfoItemStatus :: PersistOpStatus
         , pfopInfoItemStatusDesc :: Text
         , pfopInfoItemError :: Maybe Text
         , pfopInfoItemHash :: EtagHash
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
                   <*> fmap maybeNonNull (o .:? "error")
                   <*> o .: "hash"
                   <*> o .: "key"
                   <*> (fmap (/= (0 :: Int)) $ o .: "returnOld")
-- }}}1

-- | 持久化处理状态查询
persistOpsQuery :: PersistentId
                -> QiniuPfopMonad m (WsResult PersistOpInfo)
-- {{{1
persistOpsQuery pid = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  let opts0 = defaults & param "id" .~ [unPersistentId pid]

  opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path opts0
  (asWsResponseNormal' =<<) $
    ExceptT $ liftIO $ try $ WS.getWith opts sess url
  where
    url_path = "/status/get/prefop"
    url = persistOpApiUrl $ C8.unpack url_path
-- }}}1



-- vim: set foldmethod=marker:
