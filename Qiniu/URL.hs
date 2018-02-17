module Qiniu.URL where

-- {{{1 imports
import ClassyPrelude

import Qiniu.Types
import Qiniu.Security
import Qiniu.Config
-- }}}1


resourceDownloadUrl :: IsString s
                    => Bool
                    -> Maybe Text        -- ^ domain
                    -> Bucket
                    -> ResourceKey
                    -> s
resourceDownloadUrl if_ssl m_domain bucket rkey = resourceDownloadUrl' if_ssl m_domain bucket rkey Nothing


resourceDownloadUrl' :: IsString s
                     => Bool
                     -> Maybe Text        -- ^ domain
                     -> Bucket
                     -> ResourceKey
                     -> Maybe String     -- ^ optional query string
                     -> s
-- {{{1
resourceDownloadUrl' if_ssl m_domain bucket rkey m_qs = fromString $
  mconcat
    [ if if_ssl then "https://" else "http://"
    , unpack $ fromMaybe (unBucket bucket <> ".qiniudn.com") m_domain
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
  resourceDownloadUrl (qiniuConfigSslUrl qc) m_domain bucket rkey
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
  resourceDownloadUrl' (qiniuConfigSslUrl qc) m_domain bucket rkey m_qs
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
                          -> Bool
                          -> Maybe Text        -- ^ domain
                          -> Bucket
                          -> ResourceKey
                          -> s
authedResourceDownloadUrl skey akey expiry if_ssl m_domain bucket rkey =
  fromString $ unpack $
    authedResourceDownloadUrl' skey akey expiry if_ssl m_domain bucket rkey Nothing


authedResourceDownloadUrl' :: SecretKey
                           -> AccessKey
                           -> UTCTime
                           -> Bool
                           -> Maybe Text        -- ^ domain
                           -> Bucket
                           -> ResourceKey
                           -> Maybe String     -- ^ optional query string
                           -> Text
authedResourceDownloadUrl' skey akey expiry if_ssl m_domain bucket rkey m_qs =
  authedDownloadUrl skey akey expiry $ resourceDownloadUrl' if_ssl m_domain bucket rkey m_qs


authedResourceDownloadUrlByConfig :: QiniuConfig
                                  -> UTCTime
                                  -> ResourceKey
                                  -> Maybe String
                                  -> Text
-- {{{1
authedResourceDownloadUrlByConfig qc expiry rkey m_qs =
  authedResourceDownloadUrl' skey akey expiry if_ssl m_domain bucket rkey' m_qs
  where
    skey = qiniuConfigSecretKey qc
    akey = qiniuConfigAccessKey qc
    bucket = qiniuConfigBucket qc
    if_ssl = qiniuConfigSslUrl qc
    m_domain = qiniuConfigDomain qc
    rkey' = case qiniuConfigPathPrefix qc of
              p | not (null p) -> ResourceKey $ p <> "/" <> unResourceKey rkey
                | otherwise -> rkey
-- }}}1


-- vim: set foldmethod=marker:
