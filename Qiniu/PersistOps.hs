module Qiniu.PersistOps
-- {{{1 exports
  ( PersistentId(..)
  , QiniuPfopMonad
  , PersistFop(..), SomePersistFop(..), PersistFopSeries(..), FopCmd, encodeFopCmdList
  , encodeFopToText'
  , SaveAs(..)
  , ImageView2Mode(..)
  , ImageView2Dim(..)
  , ImageView2(..)
  , AvthumbFormat
  , BitRate(..), AudioCodec(..), AudioVbr(..), AudioQuality(..), VideoCodec(..), VideoResolution(..)
  , AvthumbSubOp(..)
  , AvthumbOp(..)
  , Rotation(..)
  , AvM3u8(..), PrivateM3U8(..), HlsKeyInfo (..)
  , VFrameOp(..)
  , VSampleOp(..)
  , AvInfoAudio(..)
  , AvInfoFormat(..)
  , AvInfoVideo(..)
  , AvInfo(..)
  , ImageInfo(..)
  , WatermarkTextTiled(..), WatermarkTextBlindEncode(..)
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
-- }}}1

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens hiding (Choice)
import           Control.Monad.Reader
import           Control.Monad.Except (runExceptT, ExceptT(..))
import           Data.Aeson
import           Data.Aeson.TH (deriveJSON, fieldLabelModifier, defaultOptions)
import           Data.Aeson.Types (camelTo2, typeMismatch)
import           Data.Choice
import           Data.Default (Default(..))
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Time.Clock.POSIX
#if defined(PERSISTENT)
import           Database.Persist (PersistField)
import           Database.Persist.Sql (PersistFieldSql)
#endif

import           Network.Wreq               (defaults, param, FormParam((:=)))
import qualified Network.Wreq.Session       as WS

import Qiniu.Types
import Qiniu.Class
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


-- | 所有持久化数据处理指令
class PersistFop a where
  encodeFopToText :: a -> Text

class PersistFopPathPart a where
  encodeFopPathPart :: a -> Text

data SomePersistFop = forall a. PersistFop a => SomePersistFop a

instance PersistFop SomePersistFop where
  encodeFopToText (SomePersistFop x) = encodeFopToText x

{-# DEPRECATED encodeFopToText' "use 'SaveAs' in PersistOps instead" #-}
encodeFopToText' :: PersistFop a => a -> Maybe Entry -> Text
-- {{{1
encodeFopToText' x m_save_entry =
  case m_save_entry of
    Nothing -> s
    Just entry -> s <> "|saveas/" <> decodeUtf8 (encodedEntryUri entry)
  where
    s = encodeFopToText x
-- }}}1


-- | 串联起来的一系列指令
newtype PersistFopSeries = PersistFopSeries { unPersistFopSeries :: NonEmpty SomePersistFop }

instance PersistFop PersistFopSeries where
  encodeFopToText (PersistFopSeries lst) = intercalate "%7c" $ map encodeFopToText $ toList lst


data SaveAs = SaveAs
  { saveAsEntry           :: Entry
  , saveAsDeleteAfterDays :: Maybe Int
  }
  deriving (Show)

-- {{{1 instances
instance Default (Reader Entry SaveAs) where
  def = reader $ \ x -> SaveAs x Nothing

instance PersistFop SaveAs where
  encodeFopToText (SaveAs {..}) =
    mconcat $ catMaybes
      [ Just $ "saveas/" <> encodedEntryUri saveAsEntry
      , ("/deleteAfterDays/" <>) . tshow <$> saveAsDeleteAfterDays
      ]
-- }}}1


type FopCmd = (SomePersistFop, Maybe SaveAs)

-- | 适用于 persistentOps 字段等，但不适用于下载连接
-- 因为下载连接属于 saveas 的同步调用
-- https://developer.qiniu.com/dora/api/1305/processing-results-save-saveas#3
encodeFopCmdList :: NonEmpty FopCmd -> Text
-- {{{1
encodeFopCmdList ops =
  mconcat $ intersperse ";" $ toList $ map encode_ops ops
  where
    encode_ops (fop, m_save_as) =
      let s1 = encodeFopToText fop
       in case m_save_as of
            Nothing -> s1
            Just save_as -> s1 <> "|" <> encodeFopToText save_as
-- }}}1


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
data ImageView2 = ImageView2
  { imageView2Mode :: ImageView2Mode
  , imageView2Dim :: ImageView2Dim
  , imageView2Format :: Maybe Text  -- format
  , imageView2Interlace :: Maybe Bool    -- 是否渐进
  , imageView2Quality :: Maybe Int    -- quality
  , imageView2IgnoreError :: Maybe Bool    -- ignore error
  }


-- {{{1 instances
instance Default (Reader (ImageView2Mode, ImageView2Dim) ImageView2) where
  def = reader $ \ (mode, dim) -> ImageView2 mode dim Nothing Nothing Nothing Nothing

instance PersistFop ImageView2 where
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
            Left ttl -> "expires/ " <> tshow ttl
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


-- | 暂时不实用．见 AvInfo 注释
data ImageInfo = ImageInfo { imageInfoFormat     :: Text
                           , imageInfoWidth      :: Int
                           , imageInfoHeight     :: Int
                           , imageInfoColorModel :: Text
                           }
$(deriveJSON (defaultOptions { fieldLabelModifier = lowerFirst . drop 9 }) ''ImageInfo)


-- | 文字水印
-- https://developer.qiniu.com/dora/api/1316/image-watermarking-processing-watermark#3
data WatermarkTextTiled =
  WatermarkTextTiled
    { wmtTitledText       :: Text
    , wmtTitledFont       :: Maybe Text  -- ^ 缺省为黑体，详见支持字体列表 注意：中文水印必须指定中文字体。
    , wmtTitledFontSize   :: Maybe Text   -- ^ 水印文字大小，单位: 缇，等于1/20磅，[1, 10000), 默认值是240缇，参考DPI为72。
    , wmtTitledTextColor  :: Maybe Text   -- ^ 水印文字颜色，RGB格式，可以是颜色名称（比如red）或十六进制（比如#FF0000），参考RGB颜色编码表，缺省为黑色. （经过URL安全的Base64编码）
    , wmtTitledDissolve   :: Maybe Int   -- ^ 透明度，取值范围1-100，缺省值100（完全不透明）
    , wmtTitledRotate     :: Maybe Int   -- ^ 水印文字旋转角度，[-180, 180]， 默认为0。
    , wmtTitledFillWidth  :: Maybe Int   -- ^ 水印文字填充单元宽度，默认值为100。
    , wmtTitledFillHeight :: Maybe Int   -- ^ 水印文字填充单元高度，默认值为100。
    , wmtTitledResize     :: Maybe Double   -- ^ 水印文字填充单元缩放比例，[0.1，10]，默认为1（不缩放）。
    }
    deriving (Show)

-- {{{1 instances
instance Default (Reader Text WatermarkTextTiled) where
  def = reader $ \ x -> WatermarkTextTiled x Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance PersistFop WatermarkTextTiled where
  encodeFopToText (WatermarkTextTiled {..}) =
    mconcat $ catMaybes $
      [ Just $ "watermark/4/text/" <> base64UrlEncodeT wmtTitledText
      , ("/font/" <>) <$> wmtTitledFont
      , ("/fontsize/" <>) . tshow <$> wmtTitledFontSize
      , ("/dissolve/" <>) . tshow <$> wmtTitledDissolve
      , ("/rotate/" <>) . tshow <$> wmtTitledRotate
      , ("/uw/" <>) . tshow <$> wmtTitledFillWidth
      , ("/uh/" <>) . tshow <$> wmtTitledFillHeight
      , ("/resize/" <>) . tshow <$> wmtTitledResize
      ]
-- }}}1


-- | 文字盲水印
-- CAUTION: 1 元/千次
-- https://developer.qiniu.com/dora/api/5915/blind-watermarking-processing#3
data WatermarkTextBlindEncode =
  WatermarkTextBlindEncode
    { wmtBlindEncodeText    :: ByteString  -- ^ 只支持英文数字字符，不支持中文字符，数量上线为10
    , wmtBlindEncodeVersion :: Maybe Int -- ^ 接口版本 可选 1 或者 2, 默认为 1
    }
  deriving (Show)

-- {{{1 instances
instance Default (Reader ByteString WatermarkTextBlindEncode) where
  def = reader $ \ x -> WatermarkTextBlindEncode x Nothing

instance PersistFop WatermarkTextBlindEncode where
  encodeFopToText (WatermarkTextBlindEncode bs m_ver) =
    mconcat $ catMaybes
      [ Just "watermark/6"
      , ("/version/" <>) . tshow <$> m_ver
      , Just "/method/encode"
      , Just $ ("/text/" <>) . decodeUtf8 $ bs
      ]
-- }}}1


data PfopResp = PfopResp { unPfopResp :: PersistentId }

instance FromJSON PfopResp where
  parseJSON = withObject "PfopResp" $ \o -> PfopResp <$> o .: "persistentId"


-- | 对已有的资源执行持久化数据处理
persistOpsOnSaved :: NonEmpty FopCmd
                  -> Entry          -- ^ input resource
                  -> Maybe Text     -- ^ notify url
                  -> Maybe Pipeline -- ^ pipeline
                  -> Choice "force"
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
                  , if toBool forced
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

instance HasBucket PersistOpInfo where
  getBucket = pfopInfoInputBucket

instance HasResourceKey PersistOpInfo where
  getResourceKey = pfopInfoInputKey
-- }}}1


data PfopInfoItem =
       PfopInfoItem
         { pfopInfoItemCmd        :: Text
         , pfopInfoItemStatus     :: PersistOpStatus
         , pfopInfoItemStatusDesc :: Text
         , pfopInfoItemError      :: Maybe Text
         , pfopInfoItemHash       :: Maybe EtagHash -- exists only when op is successful
         , pfopInfoItemKey        :: Maybe ResourceKey  -- exists only when op is successful
         , pfopInfoItemReturnOld  :: Bool
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
                   <*> o .:? "hash"
                   <*> o .:? "key"
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
