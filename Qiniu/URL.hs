module Qiniu.URL where

import Prelude
import Data.String
import Data.Maybe
import Data.Time

import Qiniu.Types
import Qiniu.Security


resourceDownloadUrl :: IsString s =>
    Maybe String        -- ^ domain
    -> Bucket
    -> ResourceKey
    -> s
resourceDownloadUrl m_domain bucket rkey = fromString $
    concat
        [ "http://"
        , fromMaybe (unBucket bucket ++ ".qiniudn.com") m_domain
        , keyToUrlPath rkey
        ]


authedResourceDownloadUrl :: IsString s =>
    SecretKey
    -> AccessKey
    -> UTCTime
    -> Maybe String        -- ^ domain
    -> Bucket
    -> ResourceKey
    -> s
authedResourceDownloadUrl skey akey expiry m_domain bucket rkey =
    fromString $
        authedDownloadUrl skey akey expiry $ resourceDownloadUrl m_domain bucket rkey
