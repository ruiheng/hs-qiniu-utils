module Qiniu.PersistOps
-- {{{1 exports
  ( PersistentId(..)
  , QiniuPfopMonad
  , PersistFop(..), PersistFopPathPart(..), SomePersistFop(..), PersistFopSeries(..), FopCmd, encodeFopCmdList
  , encodeFopToText'
  , SaveAs(..)
  , ImageView2Mode(..)
  , ImageView2Dim(..)
  , ImageView2(..)
  , ImageInfo(..)
  , WatermarkTextTiled(..), WatermarkTextBlindEncode(..)
  , PersistOpStatus(..)
  , persistOpStatusFromCode
  , persistOpStatusToCode
  , persistOpStatusSucceeded
  , persistOpStatusInProgress
  , PersistOpInfo(..)
  , PfopInfoItem(..), PfopInfoItem'(..)
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
#if !MIN_VERSION_aeson(1, 4, 7)
import           Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import           Data.Aeson.Types (camelTo2)
#endif
import           Data.Aeson.TH (deriveJSON)

import           Data.Choice
import           Data.Default (Default(..))
import qualified Data.ByteString.Char8 as C8
import           Data.List.NonEmpty (NonEmpty(..))
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
  encodeFopToText (PersistFopSeries lst) = intercalate "|" $ map encodeFopToText $ toList lst


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


-- | 检查过可选字段，确定有值的情况
data PfopInfoItem' = PfopInfoItem' PfopInfoItem EtagHash ResourceKey


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
