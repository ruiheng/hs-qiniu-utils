{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Types where

-- {{{1 imports
import           ClassyPrelude
import           Data.Byteable        (Byteable(..))
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Aeson.TH as AT
import qualified Data.Text.Encoding as TE
#if defined(PERSISTENT)
import           Database.Persist (PersistField)
import           Database.Persist.Sql (PersistFieldSql)
#endif
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Aeson (FromJSON, ToJSON, toJSON, object, (.=))
import           Data.Time (NominalDiffTime, addUTCTime)
import           Network.URI (escapeURIString)
-- }}}1


#if defined(PERSISTENT)
#define DERIVE_PERSIST(x) \
deriving instance PersistField x;\
deriving instance PersistFieldSql x
#else
#define DERIVE_PERSIST(x)
#endif


newtype Bucket = Bucket { unBucket :: String }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

DERIVE_PERSIST(Bucket)


newtype ResourceKey = ResourceKey { unResourceKey :: String }
                deriving (Eq, Ord, Show, FromJSON, ToJSON)

DERIVE_PERSIST(ResourceKey)


data Scope = Scope Bucket (Maybe ResourceKey)
                deriving (Eq, Ord)

encodedScopeUri :: Scope -> ByteString
-- {{{1
encodedScopeUri (Scope bucket m_key) =
    B64U.encode $ fromString $ unBucket bucket ++
                    case m_key of
                        Just key -> ":" ++ unResourceKey key
                        Nothing -> ""
-- }}}1

-- {{{1 instances
instance Show Scope where
    show (Scope bucket m_key) =
        case m_key of
            Nothing -> unBucket bucket
            Just key -> concat [ unBucket bucket, ":", unResourceKey key ]

instance ToJSON Scope where
    toJSON = toJSON . show
-- }}}1


type Entry = (Bucket, ResourceKey)

encodedEntryUri :: Entry -> ByteString
encodedEntryUri (bucket, key) =
    B64U.encode $ TE.encodeUtf8 $ fromString $ unBucket bucket ++ ":" ++ unResourceKey key


newtype EtagHash = EtagHash { unEtagHash :: String }
  deriving (Eq, Ord, Show, FromJSON, ToJSON)

instance Byteable EtagHash where
  toBytes = fst . B16.decode . fromString . unEtagHash


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

encodeFopToText' :: PersistFop a => a -> Maybe Entry -> Text
-- {{{1
encodeFopToText' x m_save_entry =
  case m_save_entry of
    Nothing -> s
    Just entry -> s <> "|saveas/" <> decodeUtf8 (encodedEntryUri entry)
  where
    s = encodeFopToText x
-- }}}1

type FopCmd = (SomePersistFop, Maybe Entry)

encodeFopCmdList :: [FopCmd] -> Text
encodeFopCmdList ops =
  mconcat $ intersperse ";" $ map (uncurry encodeFopToText') ops

data PutPolicy =
       PutPolicy
         { ppScope :: Scope
         , ppSaveKey :: Maybe ResourceKey
         , ppDeadline :: UTCTime
         , ppPersistentOps :: [FopCmd]
         , ppPersistentNotifyUrl :: Maybe Text
         , ppPersistentPipeline :: Maybe Pipeline
         }

-- {{{1 instances
instance ToJSON PutPolicy where
    toJSON pp =
        object $ catMaybes
            [ Just $ "scope"       .= ppScope pp
            , Just $ "saveKey"     .= fmap unResourceKey (ppSaveKey pp)
            , Just $ "deadline"    .= (round $ utcTimeToPOSIXSeconds $ ppDeadline pp
                                        :: Int64)
            , Just $ "persistentOps" .= encodeFopCmdList (ppPersistentOps pp)
            , fmap ("persistentNotifyUrl" .=) (ppPersistentNotifyUrl pp)
            , fmap (("persistentPipeline" .=) . unPipeline)
                    (ppPersistentPipeline pp)
            ]
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
  return $ PutPolicy scope save_key t [] Nothing Nothing
-- }}}1


newtype SecretKey = SecretKey { unSecretKey :: ByteString }
                    deriving (Eq, Ord, Show)

newtype AccessKey = AccessKey { unAccessKey :: ByteString }
                    deriving (Eq, Ord, Show)

newtype AccessToken = AccessToken { unAccessToken :: ByteString }
                    deriving (Eq, Ord, Show)


-- | 上传文件及抓取第三方资源都返回这样的值
data UploadedFileInfo = UploadedFileInfo {
                            ufiHash     :: EtagHash
                            , ufiKey    :: ResourceKey
                        }
                        deriving (Eq, Show)

$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = toLower . drop 3}
    ''UploadedFileInfo)


-- | 存储类型
data FileStoreType = FileStoreStandard  -- ^ 标准存储
                   | FileStoreLowFreq    -- ^ 低频存储
                   deriving (Eq, Ord, Bounded, Enum)


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
keyToUrlPath :: ResourceKey -> String
-- {{{1
keyToUrlPath (ResourceKey key) = '/' : escapeURIString isSafe key
    where
      isSafe = flip notElem $ asString "?#"
-- }}}1



-- vim: set foldmethod=marker:
