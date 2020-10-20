module Qiniu.PersistOps.AudioVideo where

-- {{{1 imports
import           ClassyPrelude

import           Data.Aeson
#if !MIN_VERSION_aeson(1, 4, 7)
import           Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import           Data.Aeson.Types (camelTo2)
#endif
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (typeMismatch)

import           Data.Default (Default(..))
import           Data.Time.Clock.POSIX

import Qiniu.Types
import Qiniu.Utils
import Qiniu.PersistOps
-- }}}1


-- | 码率: 128k, 1m, etc
data BitRate = BitRateK Int
              | BitRateM Int
              deriving (Show, Eq, Ord)

instance PersistFopPathPart BitRate where
  encodeFopPathPart (BitRateK i) = tshow i <> "k"
  encodeFopPathPart (BitRateM i) = tshow i <> "m"


-- | 音频质量，取值范围为0-9（mp3），10-500（aac），仅支持mp3和aac，值越小越高。不能与上述码率参数共用。
newtype AudioVbr = AudioVbr Int
  deriving (Show, Eq, Ord)

instance PersistFopPathPart AudioVbr where
  encodeFopPathPart (AudioVbr i) = tshow i

-- /ab/<BitRate> or /aq/<AudioQuality>
data AudioQuality = AudioQualityCbr BitRate
                  | AudioQualityVbr AudioVbr
                  deriving (Show, Eq, Ord)

instance PersistFopPathPart AudioQuality where
  encodeFopPathPart (AudioQualityCbr br) = "ab/" <> encodeFopPathPart br
  encodeFopPathPart (AudioQualityVbr q) = "aq/" <> encodeFopPathPart q


-- | 音频编码方案
-- 并不完整，每个指令是否能支持所有下面页面声称支持的编码也不明确
-- 所以只定义了最早期文档就出现的三种，同时应该是最常用的
-- https://developer.qiniu.com/dora/kb/1432/avthumb-parameter-acodec-format-and-explanation
--
-- CAUTION:
-- 音频质量使用 VBR时，文档说仅支持 mp3 aac
data AudioCodec = AudioMp3
                | AudioAac
                | AudioVorbis
                deriving (Show, Eq, Ord)

instance PersistFopPathPart AudioCodec where
  encodeFopPathPart AudioMp3    = "libmp3lame"
  encodeFopPathPart AudioAac    = "libfaac"
  encodeFopPathPart AudioVorbis = "libvorbis"


-- | 视频编码方案
-- 类似 AudioCodec，这也是不完整的列表
-- https://developer.qiniu.com/dora/kb/1385/avthumb-parameter-vcodec-format-and-explanation
-- see also:
-- https://video.stackexchange.com/questions/10071/what-is-the-most-compatible-compression-codec-to-in-handbrake-use-for-video-that
data VideoCodec = VideoH264
                | VideoVp8
                deriving (Show, Eq, Ord)

instance PersistFopPathPart VideoCodec where
  encodeFopPathPart VideoH264 = "libx264"
  encodeFopPathPart VideoVp8  = "libvpx"


-- | 视频分辨率
data VideoResolution = VideoResWidthHeight Int Int
                     | VideoResWidth Int
                     | VideoResHeight Int
                     deriving (Show, Eq, Ord)

instance PersistFopPathPart VideoResolution where
  encodeFopPathPart (VideoResWidthHeight w h) = tshow w <> "x" <> tshow h
  encodeFopPathPart (VideoResWidth w)         = tshow w <> "x"
  encodeFopPathPart (VideoResHeight h)        = "x" <> tshow h


-- | 音视频处理的格式参数
type AvthumbFormat = Text

-- | 音视频处理
data AvthumbSubOp = AvthumbOpAudioQuality AudioQuality     -- ^ /aq/<AudioQuality>
                  | AvthumbOpSamplingRate Int   -- ^ 采样频率，单位HZ
                  | AvthumbOpFrameRate Int      -- ^ 视频帧率
                  | AvthumbOpVideoBitRate BitRate -- ^ 视频码率
                  | AvthumbOpVideoCodec VideoCodec    -- ^ 视频编码格式
                  | AvthumbOpAudioCodec AudioCodec    -- ^ 编码方案．如 libx264
                  | AvthumbOpSubtitleCodec Text -- ^ 字幕编码方案
                  | AvthumbOpVideoResolution VideoResolution  -- ^ 视频分辨率
                  | AvthumbOpVideoAutoscale Bool -- ^ 视频是否按原比例缩放
                  | AvthumbOpStripMeta Bool     -- ^ 是否去除 meta 信息
                  | AvthumbOpNoSubtitle Bool    -- ^ 是否去除字幕
  deriving (Show, Eq, Ord)

instance PersistFopPathPart AvthumbSubOp where
-- {{{1
  encodeFopPathPart (AvthumbOpAudioQuality q)               = encodeFopPathPart q
  encodeFopPathPart (AvthumbOpSamplingRate r)               = "ar/" <> tshow r
  encodeFopPathPart (AvthumbOpAudioCodec c)                 = "acodec/" <> encodeFopPathPart c
  encodeFopPathPart (AvthumbOpFrameRate r)                  = "r/" <> tshow r
  encodeFopPathPart (AvthumbOpVideoBitRate r)               = "vb/" <> encodeFopPathPart r
  encodeFopPathPart (AvthumbOpVideoCodec c)                 = "vcodec/" <> encodeFopPathPart c
  encodeFopPathPart (AvthumbOpSubtitleCodec t)              = "scodec/" <> t
  encodeFopPathPart (AvthumbOpVideoResolution vr)           = "s/" <> encodeFopPathPart vr
  encodeFopPathPart (AvthumbOpVideoAutoscale b)             = "autoscale/" <> if b then "1" else "0"
  encodeFopPathPart (AvthumbOpStripMeta b)                  = "stripmeta/" <> if b then "1" else "0"
  encodeFopPathPart (AvthumbOpNoSubtitle b)                 = "sn/" <> if b then "1" else "0"
-- }}}1


-- | 音视频处理完整指令
data AvthumbOp = AvthumbOp AvthumbFormat [AvthumbSubOp]

instance PersistFop AvthumbOp where
  encodeFopToText (AvthumbOp format sub_ops) =
    mconcat $ intersperse "/" $ ("avthumb/" <> format) : map encodeFopPathPart (sort sub_ops)


-- | 旋转角度的选择
data Rotation = RotateClockwiseQuarter Int  -- ^ 顺时针转多少个象限.
              | RotateAuto
              deriving (Show, Eq, Ord)


instance PersistFopPathPart Rotation where
  encodeFopPathPart (RotateClockwiseQuarter q)  = if q < 0
                                                     then encodeFopPathPart RotateAuto
                                                     else case (q `rem` 4) of
                                                            qq | qq == 0 -> encodeFopPathPart RotateAuto
                                                               | otherwise -> tshow (90 * qq)
  encodeFopPathPart RotateAuto                  = "auto"


-- | 生成m3u8
-- 虽然这命令以 avthumb 开头，但跟 AvthumbOp 相差太远
-- https://developer.qiniu.com/dora/api/1485/audio-and-video-slice
data AvM3u8 = AvM3u8
  { avm3u8Domain          :: Maybe Text
  , avm3u8AudioCbrVbr     :: Maybe AudioQuality
  , avm3u8SegmentSeconds  :: Maybe Int
  , avm3u8VideoCodec      :: Maybe VideoCodec
  , avm3u8AudioCodec      :: Maybe AudioCodec
  , avm3u8VideoResolution :: Maybe VideoResolution
  , avm3u8KeyInfo         :: Maybe HlsKeyInfo
  }

instance Default AvM3u8 where
  def = AvM3u8 def def def def def def def

data HlsKeyInfo = HlsKeyInfo
  { hlsKeyBytes :: ByteString
  -- ^ must be 16 bytes
  , hlsKeyGetUrl :: Text
  -- ^ URL that can return the key
  -- 暂时不支持加密key本身
  }

instance PersistFopPathPart HlsKeyInfo where
  encodeFopPathPart (HlsKeyInfo {..}) =
    intercalate "/" $
      [ "hlsKey/" <> base64UrlEncode hlsKeyBytes
      , "hlsKeyUrl/" <> base64UrlEncodeT hlsKeyGetUrl
      ]

instance PersistFop AvM3u8 where
  encodeFopToText (AvM3u8 {..}) =
    intercalate "/" $ catMaybes
      [ Just $ "avthumb/m3u8"
      , Just $ case avm3u8Domain of
                 Nothing -> "noDomain/1"
                 Just domain -> "domain/" <> base64UrlEncodeT domain

      , fmap encodeFopPathPart avm3u8AudioCbrVbr
      , flip fmap avm3u8SegmentSeconds $ \ t -> "segtime/" <> tshow t
      , flip fmap avm3u8VideoResolution $ \ x -> "s/" <> encodeFopPathPart x
      , ("vcodec/" <>) . encodeFopPathPart <$> avm3u8VideoCodec
      , ("acodec/" <>) . encodeFopPathPart <$> avm3u8AudioCodec
      , fmap encodeFopPathPart avm3u8KeyInfo
      ]

-- | 私有M3U8（pm3u8）
-- https://developer.qiniu.com/dora/api/1292/private-m3u8-pm3u8
data PrivateM3U8 = PrivateM3U8
  { pm3u8TtlDeadline :: Either Int POSIXTime
  }

-- XXX: 这看上去很像 PersistFop 但只用在下载 m3u8 文件作为 QS的一段
-- 因此我们只实现 PersistFopPathPart 接口
instance QueryStringSegment PrivateM3U8 where
  toQsSegment (PrivateM3U8 {..}) =
    intercalate "/" $ catMaybes
      [ Just "pm3u8"
      , Just "0" -- mode
      , Just $
          case pm3u8TtlDeadline of
            Left ttl -> "expires/" <> tshow ttl
            Right deadline -> "deadline/" <> tshow (round deadline :: Integer)
      ]


-- | 视频帧缩略图
data VFrameOp = VFrameOp Text Float (Maybe VideoResolution) (Maybe Rotation)
  deriving (Show, Eq, Ord)

instance PersistFop VFrameOp where
-- {{{1
  encodeFopToText (VFrameOp fmt offset m_vres m_rotate) =
    intercalate "/" $ catMaybes
      [ Just $ "vframe/" <> fmt
      , Just $ "offset/" <> tshow offset
      , flip fmap m_vres $
          \ case VideoResWidthHeight w h -> "w/" <> tshow w <> "/h/" <> tshow h
                 VideoResWidth w         -> "w/" <> tshow w
                 VideoResHeight h        -> "h/" <> tshow h
      , flip fmap m_rotate $ \ r -> "rotate/" <> encodeFopPathPart r
      ]
    where
-- }}}1


-- | 视频采样缩略图
data VSampleOp = VSampleOp
                  Text    -- format
                  Text    -- pattern
                  Float   -- start time
                  Float   -- duration
                  (Maybe VideoResolution) -- doc says: <width>x<height>
                  (Maybe Rotation)
                  (Maybe Float)

instance PersistFop VSampleOp where
-- {{{1
  encodeFopToText (VSampleOp format pattern start_time duration m_vres m_rotate m_interval) =
    intercalate "/" $ catMaybes
      [ Just $ "vsample/" <> format
      , Just $ "ss/" <> tshow start_time
      , Just $ "t/" <> tshow duration
      , flip fmap m_vres $ \ res -> "s/" <> encodeFopPathPart res
      , flip fmap m_rotate $ \ r -> "rotate/" <> encodeFopPathPart r
      , flip fmap m_interval $ \ iv -> "interval/" <> tshow iv
      , Just $ "pattern/" <> base64UrlEncodeT pattern
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




-- vim: set foldmethod=marker:
