module Qiniu.URL where

-- {{{1 imports
import ClassyPrelude

import Qiniu.Types
import Qiniu.Security
import Qiniu.Config
-- }}}1


resourceDownloadUrl :: IsString s
                    => Maybe String        -- ^ domain
                    -> Bucket
                    -> ResourceKey
                    -> s
resourceDownloadUrl m_domain bucket rkey = resourceDownloadUrl' m_domain bucket rkey Nothing


resourceDownloadUrl' :: IsString s
                     => Maybe String        -- ^ domain
                     -> Bucket
                     -> ResourceKey
                     -> Maybe String     -- ^ optional query string
                     -> s
-- {{{1
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
-- }}}1


resourceDownloadUrlByConfig :: IsString s
                            => QiniuConfig
                            -> ResourceKey
                            -> s
-- {{{1
resourceDownloadUrlByConfig qc rkey =
  resourceDownloadUrl m_domain bucket rkey
  where
    m_domain = qiniuConfigDomain qc
    bucket = qiniuConfigBucket qc
-- }}}1


resourceDownloadUrlByConfig' :: IsString s
                             => QiniuConfig
                             -> ResourceKey
                             -> Maybe String     -- ^ optional query string
                             -> s
-- {{{1
resourceDownloadUrlByConfig' qc rkey m_qs =
  resourceDownloadUrl' m_domain bucket rkey m_qs
  where
    m_domain = qiniuConfigDomain qc
    bucket = qiniuConfigBucket qc
-- }}}1


resourceDownloadUrlByPubConfig :: IsString s
                               => QiniuDualConfig
                               -> ResourceKey
                               -> s
resourceDownloadUrlByPubConfig dual =
  resourceDownloadUrlByConfig (pubOfQiniuDualConfig dual)


resourceDownloadUrlByPubConfig' :: IsString s
                                => QiniuDualConfig
                                -> ResourceKey
                                -> Maybe String     -- ^ optional query string
                                -> s
resourceDownloadUrlByPubConfig' dual = resourceDownloadUrlByConfig' (pubOfQiniuDualConfig dual)


authedResourceDownloadUrl :: IsString s
                          => SecretKey
                          -> AccessKey
                          -> UTCTime
                          -> Maybe String        -- ^ domain
                          -> Bucket
                          -> ResourceKey
                          -> s
authedResourceDownloadUrl skey akey expiry m_domain bucket rkey =
  authedResourceDownloadUrl' skey akey expiry m_domain bucket rkey Nothing


authedResourceDownloadUrl' :: IsString s
                           => SecretKey
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



-- vim: set foldmethod=marker:
