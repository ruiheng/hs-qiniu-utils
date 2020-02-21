module Qiniu.URL where

-- {{{1 imports
import ClassyPrelude
import Control.Monad.State.Strict (execState, get, put)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock.POSIX
import qualified Data.Text as T

import Qiniu.Types
import Qiniu.PersistOps
import Qiniu.Upload
import Qiniu.Security
import Qiniu.Config
-- }}}1


-- 处理结果另存: query string 中 saveas/xxx/sign/<sign> 部分的算法
queryStringFopSaveAs :: (SecretKey, AccessKey) -> Entry -> Text -> Text
-- {{{1
queryStringFopSaveAs (skey, akey) entry url =
  url_to_sign <> "/sign/" <> unAccessKey akey <> ":" <> sign_str
  where
    url_to_sign = url <> "|saveas/" <> encodedEntryUri entry
    sign_str = encodedSign' skey (encodeUtf8 url_to_sign)
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


-- | 生成公开资源下载连接的完整版本逻辑
-- XXX: 这虽然看上去是个下载URL，但实际上有 saveas 响应其实是个JSON文档
--      响应是这样的: {"key":"xxxx","hash":"FjkUApzF4w8zPEd_e7_lgDR2WkI_"}
resourceDownloadUrlX :: Bool
                     -> Maybe Text        -- ^ domain
                     -> Entry
                     -> Maybe String     -- ^ optional query string
                     -> Maybe ((SecretKey, AccessKey), NonEmpty FopCmd)
                     -- ^ secret key is needed, because of 'saveas'
                     -> Text
-- {{{1
resourceDownloadUrlX if_ssl m_domain (bucket, rkey) m_qs m_secret_cmds =
  case qs' of
    "" -> pre_url_with_scheme
    _  -> append_sep pre_url_with_scheme <> fromString qs'

  where
    pre_url_with_scheme =
      mconcat
        [ if if_ssl then "https://" else "http://"
        , pre_url_without_scheme_with_fop
        ]

    pre_url_no_scheme = mconcat
      [ fromMaybe fallback_domain m_domain
      , keyToUrlPath rkey
      ]

    get_sep prev_part =
      if '?' `elem` prev_part
         then "&"
         else "?"

    append_sep prev_part = prev_part <> get_sep prev_part

    pre_url_without_scheme_with_fop =
      case m_secret_cmds of
        Nothing -> pre_url_no_scheme
        Just ((skey, akey), cmds) ->
          flip execState (append_sep pre_url_no_scheme) $ do
            let fop_sep = "%7c" -- XXX: 不要直接使用 | ，因为有些库函数发送请求时自动会把它变成 %7c，就会出错
            forM_ cmds $ \ (fop, m_saveas) -> do
              let fop_str = T.replace "|" "%7c" $ encodeFopToText fop
              s <- get
              let s' = if "?" `isSuffixOf` s
                          then s
                          else s <> fop_sep
              let prev_url = s' <> fop_str

              case m_saveas of
                Nothing -> put prev_url
                Just save_as -> do
                  let sign_url_part = prev_url <> fop_sep <> "saveas/" <> encodedEntryUri (saveAsEntry save_as)
                  let sign_str = encodedSign' skey $ encodeUtf8 sign_url_part
                  put $ mconcat $ catMaybes
                        [ Just sign_url_part
                        , Just $ "/sign/" <> unAccessKey akey <> ":" <> sign_str
                        , ("/deleteAfterDays/" <>) . tshow <$> saveAsDeleteAfterDays save_as
                        ]

    fallback_domain = unBucket bucket <> ".qiniudn.com"

    qs' = case m_qs of
            Nothing       -> ""
            Just ('?':qs) -> qs
            Just qs       -> qs
-- }}}1


resourceDownloadUrlByConfig :: IsString s
                            => QiniuBucketConfig
                            -> ResourceKey
                            -> s
-- {{{1
resourceDownloadUrlByConfig qc rkey =
  resourceDownloadUrl (qnBucketSslUrl qc) m_domain bucket rkey
  where
    m_domain = qnBucketDomain qc
    bucket = qnBucketName qc
-- }}}1


resourceDownloadUrlByConfig' :: IsString s
                             => QiniuBucketConfig
                             -> ResourceKey
                             -> Maybe String     -- ^ optional query string
                             -> s
-- {{{1
resourceDownloadUrlByConfig' qc rkey m_qs =
  resourceDownloadUrl' (qnBucketSslUrl qc) m_domain bucket rkey m_qs
  where
    m_domain = qnBucketDomain qc
    bucket = qnBucketName qc
-- }}}1


resourceDownloadUrlByPubConfig :: IsString s
                               => QiniuDualBucketConfig
                               -> ResourceKey
                               -> s
resourceDownloadUrlByPubConfig dual =
  resourceDownloadUrlByConfig (pubOfQiniuDualBucketConfig dual)


resourceDownloadUrlByPubConfig' :: IsString s
                                => QiniuDualBucketConfig
                                -> ResourceKey
                                -> Maybe String     -- ^ optional query string
                                -> s
resourceDownloadUrlByPubConfig' dual = resourceDownloadUrlByConfig' (pubOfQiniuDualBucketConfig dual)


authedDownloadUrl :: SecretKey
                  -> AccessKey
                  -> UTCTime
                  -> Text
                  -> Text
-- {{{1
authedDownloadUrl skey akey expiry url =
    url2 <> "&token=" <> unDownloadToken token
    where
        encoded_sign    = encodedSign' skey $ encodeUtf8 url2

        e               = if '?' `elem` url
                            then "&e="
                            else "?e="

        url2            = mconcat
                            [ url
                            , e
                            , tshow (round $ utcTimeToPOSIXSeconds expiry :: Int64)
                            ]

        token           = DownloadToken $ mconcat
                            [ unAccessKey akey
                            , ":"
                            , encoded_sign
                            ]
-- }}}1


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


-- | 下载资源连接的完整逻辑
-- XXX: 这虽然看上去是个下载URL，但实际上有 saveas 响应其实是个JSON文档
--      响应是这样的: {"key":"xxxx","hash":"FjkUApzF4w8zPEd_e7_lgDR2WkI_"}
--      见 resourceDownloadUrlX 说明
authedResourceDownloadUrlX :: SecretKey
                           -> AccessKey
                           -> UTCTime
                           -> Bool
                           -> Maybe Text        -- ^ domain
                           -> Entry
                           -> Maybe String     -- ^ optional query string
                           -> Maybe (NonEmpty FopCmd)
                           -> Text
authedResourceDownloadUrlX skey akey expiry if_ssl m_domain entry m_qs m_cmds =
  authedDownloadUrl skey akey expiry $ resourceDownloadUrlX if_ssl m_domain entry m_qs (fmap ((skey, akey),) m_cmds)


authedResourceDownloadUrlByConfig :: QiniuAccountConfig
                                  -> QiniuBucketConfig
                                  -> UTCTime
                                  -> ResourceKey
                                  -> Maybe String
                                  -> Text
-- {{{1
authedResourceDownloadUrlByConfig qa qc expiry rkey m_qs =
  authedResourceDownloadUrl' skey akey expiry if_ssl m_domain bucket rkey' m_qs
  where
    skey = qnAccountSecretKey qa
    akey = qnAccountAccessKey qa
    bucket = qnBucketName qc
    if_ssl = qnBucketSslUrl qc
    m_domain = qnBucketDomain qc
    rkey' = case qnBucketPathPrefix qc of
              p | not (null p) -> ResourceKey $ p <> "/" <> unResourceKey rkey
                | otherwise -> rkey
-- }}}1


-- vim: set foldmethod=marker:
