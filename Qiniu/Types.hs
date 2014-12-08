{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Qiniu.Types where

import Prelude
import qualified Data.ByteString.Base64.URL as B64U

import Data.Time                            (UTCTime)
import Data.Time.Clock.POSIX                (utcTimeToPOSIXSeconds)
import Data.Aeson                           (FromJSON, ToJSON, toJSON, object, (.=))
import Data.Int                             (Int64)
import Data.ByteString                      (ByteString)
import Data.Time                            (getCurrentTime, NominalDiffTime
                                            , addUTCTime)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Data.String                          (IsString)
import Data.List.Split                      (splitWhen)
import Data.String                          (fromString)
import Network.URI                          (isUnreserved, escapeURIString)


newtype Bucket = Bucket { unBucket :: String }
                deriving (Eq, Ord, Show)

newtype ResourceKey = ResourceKey { unResourceKey :: String }
                deriving (Eq, Ord, Show, FromJSON, ToJSON)

data Scope = Scope Bucket (Maybe ResourceKey)
                deriving (Eq, Ord)

instance Show Scope where
    show (Scope bucket m_key) =
        case m_key of
            Nothing -> unBucket bucket
            Just key -> concat [ unBucket bucket, ":", unResourceKey key ]

instance ToJSON Scope where
    toJSON = toJSON . show


type Entry = (Bucket, ResourceKey)

encodedEntryUri :: Entry -> ByteString
encodedEntryUri (bucket, key) =
    B64U.encode $ fromString $ unBucket bucket ++ ":" ++ unResourceKey key



data PutPolicy = PutPolicy {
                    ppScope             :: Scope
                    , ppSaveKey         :: Maybe ResourceKey
                    , ppDeadline        :: UTCTime
                }

instance ToJSON PutPolicy where
    toJSON pp =
        object
            [ "scope"       .= ppScope pp
            , "saveKey"     .= fmap unResourceKey (ppSaveKey pp)
            , "deadline"    .= (round $ utcTimeToPOSIXSeconds $ ppDeadline pp :: Int64)
            ]


mkPutPolicy :: MonadIO m =>
    Scope
    -> Maybe ResourceKey    -- ^ the 'saveKey' field
    -> NominalDiffTime -> m PutPolicy
mkPutPolicy scope save_key dt = liftIO $ do
    now <- getCurrentTime
    let t = addUTCTime dt now
    return $ PutPolicy scope save_key t

newtype SecretKey = SecretKey { unSecretKey :: ByteString }
                    deriving (Eq, Ord, Show)

newtype AccessKey = AccessKey { unAccessKey :: ByteString }
                    deriving (Eq, Ord, Show)

newtype AccessToken = AccessToken { unAccessToken :: ByteString }
                    deriving (Eq, Ord, Show)


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
keyToUrlPath :: ResourceKey -> String
keyToUrlPath (ResourceKey key) = '/' : esc parts
    where
        parts = splitWhen (== '/') key

        esc []      = ""
        esc ("":[]) = ""
        esc (x:xs)  =   let t = esc xs
                            s = if null x
                                    then "%2F"
                                    else escapeURIString isUnreserved x ++
                                            (if null t && xs /= [""] then "" else "/")
                         in s ++ t
