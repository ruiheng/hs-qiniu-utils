{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Types where

-- {{{1 imports
import           ClassyPrelude
import           Data.Byteable        (Byteable(..))
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Aeson.Text as A
import qualified Data.Aeson.TH as AT
import           Data.List.NonEmpty (NonEmpty)
#if defined(PERSISTENT)
import           Database.Persist (PersistField)
import           Database.Persist.Sql (PersistFieldSql)
#endif
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Aeson (FromJSON, ToJSON, toJSON, object, (.=))
import           Data.Time (NominalDiffTime, addUTCTime)
import           Network.URI (escapeURIString)

import Qiniu.Utils
-- }}}1


#if defined(PERSISTENT)
#define DERIVE_PERSIST(x) \
deriving instance PersistField x;\
deriving instance PersistFieldSql x
#else
#define DERIVE_PERSIST(x)
#endif


newtype Bucket = Bucket { unBucket :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

DERIVE_PERSIST(Bucket)


newtype ResourceKey = ResourceKey { unResourceKey :: Text }
                deriving (Eq, Ord, Show, FromJSON, ToJSON, Monoid, Semigroup)

DERIVE_PERSIST(ResourceKey)


data Scope = Scope Bucket (Maybe ResourceKey)
                deriving (Eq, Ord)

encodedScopeUri :: Scope -> ByteString
-- {{{1
encodedScopeUri (Scope bucket m_key) =
    B64U.encode $ encodeUtf8 $ unBucket bucket <>
                    case m_key of
                        Just key -> ":" <> unResourceKey key
                        Nothing -> ""
-- }}}1

-- {{{1 instances
instance Show Scope where
    show (Scope bucket m_key) =
      unpack $
        case m_key of
            Nothing -> unBucket bucket
            Just key -> mconcat [ unBucket bucket, ":", unResourceKey key ]

instance ToJSON Scope where
    toJSON = toJSON . show
-- }}}1


type Entry = (Bucket, ResourceKey)

showEntry :: Entry -> Text
showEntry (bucket, rkey) = mconcat [ "(", unBucket bucket, ", ", unResourceKey rkey, ")" ]

encodedEntryUri :: IsString s => Entry -> s
encodedEntryUri (bucket, key) =
  base64UrlEncode $ encodeUtf8 $ unBucket bucket <> ":" <> unResourceKey key


newtype EtagHash = EtagHash { unEtagHash :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance Byteable EtagHash where
  toBytes = encodeUtf8 . unEtagHash


-- | 持久化数据处理的队列
newtype Pipeline = Pipeline { unPipeline :: Text }
                        deriving (Eq, Ord, Show, FromJSON, ToJSON)

DERIVE_PERSIST(Pipeline)


-- | 所有持久化数据处理指令
class PersistFop a where
  encodeFopToText :: a -> Text

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


type FopCmd = (SomePersistFop, Maybe Entry)

encodeFopCmdList :: [FopCmd] -> Text
encodeFopCmdList ops =
  mconcat $ intersperse ";" $ map (uncurry encodeFopToText') ops


data CallbackBodyType = CbQueryString
                      | CbJson
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- | 存储类型
data FileStoreType = FileStoreStandard  -- ^ 标准存储
                   | FileStoreLowFreq    -- ^ 低频存储
                   deriving (Eq, Ord, Bounded, Enum)

data PutPolicy =
       PutPolicy
         { ppScope               :: Scope
         , ppSaveKey             :: Maybe ResourceKey
         , ppDeadline            :: UTCTime
         , ppIsPrefixalScope     :: Maybe Bool
         , ppInsertOnly          :: Maybe Bool
         , ppEndUser             :: Maybe Text
         , ppReturnUrl           :: Maybe Text
         , ppReturnBody          :: Maybe (Map Text Text)
         , ppCallbackUrls        :: [Text]
         , ppCallbackHost        :: Maybe Text
         , ppCallbackBody        :: Maybe (Map Text Text)
         , ppCallbackBodyType    :: Maybe CallbackBodyType
         , ppPersistentOps       :: [FopCmd]
         , ppPersistentNotifyUrl :: Maybe Text
         , ppPersistentPipeline  :: Maybe Pipeline
         , ppFileSizeMin         :: Maybe Int64
         , ppFileSizeLimit       :: Maybe Int64
         , ppDetectMime          :: Maybe Bool
         , ppMimeLimit           :: Maybe Text
         , ppDeleteAfterDays     :: Maybe Int
         -- ^ deleteAfterDays 的逻辑现在的文档并不记录
         -- 但在网上的代码，及官方js-sdk的代码中都可以看到
         -- 不确定官方是打算删除这个字段还是目前文档的错误
         , ppFileStoreType       :: Maybe FileStoreType
         }

-- {{{1 instances
instance ToJSON PutPolicy where
    toJSON pp =
        object $ catMaybes
            [ Just $ "scope"       .= ppScope pp
            , Just $ "saveKey"     .= fmap unResourceKey (ppSaveKey pp)
            , Just $ "deadline"    .= (round $ utcTimeToPOSIXSeconds $ ppDeadline pp :: Int64)
            , fmap (("isPrefixalScope" .=) . fromEnum) (ppIsPrefixalScope pp)
            , fmap (("insertOnly" .=) . fromEnum) (ppInsertOnly pp)
            , fmap ("endUser" .=) (ppEndUser pp)
            , fmap ("returnUrl" .=) (ppReturnUrl pp)
            , fmap ("returnBody" .=) (map_to_qs <$> ppReturnBody pp)

            , if null cb_urls
                 then Nothing
                 else Just $ "callbackUrl" .= intercalate ";" cb_urls

            , if null cb_urls
                 then Nothing
                 else fmap ("callbackHost" .=) (ppCallbackHost pp)

            , if null cb_urls
                 then Nothing
                 else case effective_callback_body_type of
                        CbQueryString -> fmap ("callbackBody" .=) cb_var_map_qs
                        CbJson -> ("callbackBody" .=) . A.encodeToLazyText <$> cb_var_map_json

            , if null cb_urls
                 then Nothing
                 else flip fmap (ppCallbackBodyType pp) $ \ t ->
                        case t of
                          CbQueryString -> "callbackBodyType" .= asText "application/x-www-form-urlencoded"
                          CbJson -> "callbackBodyType" .= asText "application/json"

            , case encodeFopCmdList (ppPersistentOps pp) of
                t | not (null t) -> Just $ "persistentOps" .= t
                  | otherwise    -> Nothing

            , fmap ("persistentNotifyUrl" .=) (ppPersistentNotifyUrl pp)
            , fmap (("persistentPipeline" .=) . unPipeline)
                    (ppPersistentPipeline pp)

            , fmap ("fsizeMin" .=) (ppFileSizeMin pp)
            , fmap ("fsizeLimit" .=) (ppFileSizeLimit pp)
            , fmap ("detectMime" .=) (ppDetectMime pp)
            , fmap ("mimeLimit" .=) (ppMimeLimit pp)
            , fmap ("deleteAfterDays" .=) (ppDeleteAfterDays pp)
            , fmap ("fileType" .=) (fromEnum <$> ppFileStoreType pp)
            ]
        where
          cb_urls = ppCallbackUrls pp
          cb_var_map = ppCallbackBody pp

          map_to_qs :: Map Text Text -> Text
          map_to_qs m = intercalate "&" $ flip map (mapToList m) $ \ (k, v) -> k <> "=" <> v

          cb_var_map_qs = map_to_qs <$> cb_var_map
          cb_var_map_json = cb_var_map
          effective_callback_body_type = fromMaybe CbQueryString $ ppCallbackBodyType pp
-- }}}1


mkPutPolicy :: MonadIO m
            => Scope
            -> Maybe ResourceKey    -- ^ the 'saveKey' field
            -> NominalDiffTime
            -> m PutPolicy
-- {{{1
mkPutPolicy scope save_key dt = liftIO $ do
  now <- getCurrentTime
  let t = addUTCTime dt now
  return $ PutPolicy scope save_key t
            Nothing
            Nothing Nothing
            Nothing Nothing
            [] Nothing Nothing Nothing
            [] Nothing Nothing
            Nothing Nothing Nothing Nothing Nothing Nothing
-- }}}1


newtype SecretKey = SecretKey { unSecretKey :: Text }
                    deriving (Eq, Ord, Show)

newtype AccessKey = AccessKey { unAccessKey :: Text }
                    deriving (Eq, Ord, Show)

newtype AccessToken = AccessToken { unAccessToken :: Text }
                    deriving (Eq, Ord, Show)


-- | 上传文件及抓取第三方资源都返回这样的值
-- XXX: 实际上，根据上传策略的文档，上传结果的返回内容受 returnBody 影响
--      读 js-sdk 代码中的node.js服务器端代码也反映了这个逻辑
-- 所以以下这个类型只能说是未指定 returnBody 时的结果
data UploadedFileInfo = UploadedFileInfo {
                            ufiHash     :: EtagHash
                            , ufiKey    :: ResourceKey
                        }
                        deriving (Eq, Show)

$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = toLower . drop 3}
    ''UploadedFileInfo)



logSource :: IsString a => a
logSource = "QiNiu"


-- | 大部分时候，资源 key 的字串直接就可以拼接在 URL 里
-- 但有些时候要做 url 转义
-- 见 http://kb.qiniu.com/52slk76w
-- 这些情况包括：
-- 首字符是 /
-- 连续出现的 /
-- 其它特殊字符 ? & 之类
-- 这些特殊情况是本来就应该避免的。
-- 这里的算法与文档所述并不完全一致，但按逻辑推理应该是可用的。
-- 这个函数的效果是让以下的恒等式成立。
-- unEscapeString (keyToUrlPath (ResourceKey k)) == '/' : k
-- XXX: 上面的文档已经过时．七牛上另有一篇类似的文档，但是似乎也是过时的
-- 七牛官方的qshell工具并没有使用这个转义法．
-- 根据qshell的代码（src/qiniu/api.v6/url/urlescape.go
-- 只需要转义`?`即可．实测`#`也需要转义
-- 七牛签名要求的url是要恰到好处的转义，完全没转义的不能用，完全转义的也不能用
keyToUrlPath :: IsString s => ResourceKey -> s
-- {{{1
keyToUrlPath (ResourceKey key) = fromString $ '/' : escapeURIString isSafe (unpack key)
    where
      isSafe = flip notElem $ asString "?#"
-- }}}1



-- vim: set foldmethod=marker:
