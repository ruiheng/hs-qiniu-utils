{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Types where

-- {{{1 imports
import           ClassyPrelude
import           Data.Byteable        (Byteable(..))
import qualified Data.ByteString.Base64.URL as B64U
#if defined(PERSISTENT)
import           Database.Persist (PersistField)
import           Database.Persist.Sql (PersistFieldSql)
#endif
import           Data.Aeson (FromJSON, ToJSON, toJSON)
import           Network.URI (escapeURIString)
import           Network.HTTP (urlEncode)
import           Web.PathPieces (PathPiece(..))

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
    B64U.encodeBase64' $ encodeUtf8 $ unBucket bucket <>
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

instance PathPiece EtagHash where
  toPathPiece = unEtagHash
  fromPathPiece = fmap EtagHash . fromPathPiece


-- | 持久化数据处理的队列
newtype Pipeline = Pipeline { unPipeline :: Text }
                        deriving (Eq, Ord, Show, FromJSON, ToJSON)

DERIVE_PERSIST(Pipeline)


data CallbackBodyType = CbQueryString
                      | CbJson
                      deriving (Show, Eq, Ord, Enum, Bounded)

-- | 存储类型
data FileStoreType = FileStoreStandard  -- ^ 标准存储
                   | FileStoreLowFreq    -- ^ 低频存储
                   deriving (Eq, Ord, Bounded, Enum)

newtype SecretKey = SecretKey { unSecretKey :: Text }
                    deriving (Eq, Ord, Show)

newtype AccessKey = AccessKey { unAccessKey :: Text }
                    deriving (Eq, Ord, Show)

newtype AccessToken = AccessToken { unAccessToken :: Text }
                    deriving (Eq, Ord, Show)


class QueryStringSegment a where
  toQsSegment :: a -> Text

instance QueryStringSegment (Text, Text) where
  toQsSegment = toQsSegment . (unpack *** unpack)

instance QueryStringSegment (String, String) where
  toQsSegment (n, v) = pack $ urlEncode n <> "=" <> urlEncode v


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
