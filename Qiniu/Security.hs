{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Security where

-- {{{1 imports
import           ClassyPrelude
import           Control.Lens
import qualified Crypto.Hash.SHA1 as SHA1
import           Crypto.MAC.HMAC (hmac)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BBU8
import           Network.HTTP.Client (Request, defaultRequest)
import           Network.HTTP.Types (Header, hAuthorization, renderQueryText)
import qualified Network.Wreq as W
import           Network.Wreq.Types (Postable, postPayload)
import qualified Network.HTTP.Client as HC

import           Qiniu.Types
import           Qiniu.HttpClient (replaceReqHttpHeader, requestBodyToBsBuilder)
-- }}}1


sign :: SecretKey -> ByteString -> ByteString
sign = hmac SHA1.hash 64 . encodeUtf8 . unSecretKey

encodedSign :: SecretKey -> ByteString -> ByteString
encodedSign skey = B64U.encode . sign skey

encodedSign' :: IsString s => SecretKey -> ByteString -> s
encodedSign' skey = fromString . C8.unpack . encodedSign skey


-- | 创建 Access Token 的算法
mkAccessToken :: SecretKey
              -> AccessKey
              -> ByteString       -- ^ url path
              -> ByteString       -- ^ query string
              -> LB.ByteString    -- ^ body bytestring
              -> AccessToken
-- {{{1
mkAccessToken secret_key access_key path qs body_bs =
    AccessToken $ mconcat
        [ unAccessKey access_key
        , ":"
        , encodedSign' secret_key $ LB.toStrict sign_str
        ]
    where
        qm  = fromIntegral $ fromEnum '?'
        qs' = if not (B.null qs) && B.head qs /= qm
                then B.cons qm qs
                else qs

        path_qs     = path <> qs'

        sign_str    = BB.toLazyByteString $ mconcat
                        [ BB.fromByteString path_qs
                        , BBU8.fromChar '\n'
                        , BB.fromLazyByteString body_bs
                        ]
-- }}}1


-- | 根据已知的 Http Request 计算 Access Token
mkAccessTokenFromReq :: SecretKey
                     -> AccessKey
                     -> Request
                     -> IO AccessToken
-- {{{1
mkAccessTokenFromReq secret_key access_key req = do
    body_bb <- requestBodyToBsBuilder body
    let body_bs = BB.toLazyByteString body_bb

    return $ mkAccessToken
                secret_key access_key
                path qs
                body_bs
    where
        body    = HC.requestBody req
        path    = HC.path req
        qs      = HC.queryString req
-- }}}1


accessTokenHeader :: AccessToken -> Header
accessTokenHeader atoken = (hAuthorization, "QBox " <> encodeUtf8 (unAccessToken atoken))

setAccessTokenHeaderOptions :: AccessToken -> W.Options -> W.Options
setAccessTokenHeaderOptions atoken = W.header hAuthorization .~ [ ("QBox " <> encodeUtf8 (unAccessToken atoken)) ]

-- | 在 Http Request 里加入 Access Token 相关的头部信息
applyAccessTokenForReq :: SecretKey
                       -> AccessKey
                       -> Request
                       -> IO Request
-- {{{1
applyAccessTokenForReq secret_key access_key req = do
  token <- mkAccessTokenFromReq secret_key access_key req
  return $ replaceReqHttpHeader (accessTokenHeader token) req
-- }}}1


-- | 为 POST 请求，在 Http Request 里加入 Access Token 相关的头部信息 Authorization
-- 注意：因为计算 Authorization 头信息与请求的 query string 相关，这里假定 query string 已全部体现在 Options 里
--       即 opts ^. params 已是全部要发出的 query string 内容
--       即 应在所以有 param "xxx" .~ [ "yyy" ] 之类的调用之后再使用这个函数
applyAccessTokenPost :: Postable a
                     => SecretKey
                     -> AccessKey
                     -> ByteString
                     -> Maybe a
                     -> W.Options
                     -> IO W.Options
-- {{{1
applyAccessTokenPost secret_key access_key url_path m_p opts = do
  req <- mk_req init_req
  token <- mkAccessTokenFromReq secret_key access_key req
  return $ setAccessTokenHeaderOptions token opts
  where
    mk_req = case m_p of
               Just p -> postPayload p
               Nothing -> postPayload (mempty :: ByteString)

    qs_params = opts ^. W.params
    init_req = defaultRequest { HC.method = "POST"
                              , HC.queryString = toStrict $ BB.toLazyByteString $ renderQueryText True $ map (second Just) qs_params
                              , HC.path = url_path
                              }
-- }}}1


-- | 为 GET 请求，在 Http Request 里加入 Access Token 相关的头部信息 Authorization
-- 注意：因为计算 Authorization 头信息与请求的 query string 相关，这里假定 query string 已全部体现在 Options 里
--       即 opts ^. params 已是全部要发出的 query string 内容
--       即 应在所以有 param "xxx" .~ [ "yyy" ] 之类的调用之后再使用这个函数
applyAccessTokenGet :: SecretKey
                    -> AccessKey
                    -> ByteString
                    -> W.Options
                    -> IO W.Options
-- {{{1
applyAccessTokenGet secret_key access_key url_path opts = do
  token <- mkAccessTokenFromReq secret_key access_key init_req
  return $ setAccessTokenHeaderOptions token opts
  where
    qs_params = opts ^. W.params
    init_req = defaultRequest { HC.method = "GET"
                              , HC.queryString = toStrict $ BB.toLazyByteString $ renderQueryText True $ map (second Just) qs_params
                              , HC.path = url_path
                              }
-- }}}1



-- vim: set foldmethod=marker:
