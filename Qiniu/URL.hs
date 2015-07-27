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
resourceDownloadUrl m_domain bucket rkey = resourceDownloadUrl' m_domain bucket rkey Nothing


resourceDownloadUrl' :: IsString s =>
    Maybe String        -- ^ domain
    -> Bucket
    -> ResourceKey
    -> Maybe String     -- ^ optional query string
    -> s
resourceDownloadUrl' m_domain bucket rkey m_qs = fromString $
    concat
        [ "http://"
        , fromMaybe (unBucket bucket ++ ".qiniudn.com") m_domain
        , keyToUrlPath rkey
        , case m_qs of
            Nothing         -> ""
            Just qs@('?':_) -> qs
            Just qs         -> '?' : qs
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
    authedResourceDownloadUrl' skey akey expiry m_domain bucket rkey Nothing

authedResourceDownloadUrl' :: IsString s =>
    SecretKey
    -> AccessKey
    -> UTCTime
    -> Maybe String        -- ^ domain
    -> Bucket
    -> ResourceKey
    -> Maybe String     -- ^ optional query string
    -> s
authedResourceDownloadUrl' skey akey expiry m_domain bucket rkey m_qs =
    fromString $
        authedDownloadUrl skey akey expiry $ resourceDownloadUrl' m_domain bucket rkey m_qs
