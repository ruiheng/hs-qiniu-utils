{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Security where

import Prelude
import Data.ByteString                      (ByteString)
import qualified Crypto.Hash.SHA1           as SHA1
import Crypto.MAC.HMAC                      (hmac)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.Aeson                 as A
import qualified Data.ByteString.UTF8       as UTF8
import qualified Blaze.ByteString.Builder   as BB
import qualified Blaze.ByteString.Builder.Char.Utf8 as BBU8
import Data.Time.Clock.POSIX                (utcTimeToPOSIXSeconds)
import Data.Time                            (UTCTime)
import Data.Int                             (Int64)
import Data.Monoid                          (mconcat, (<>))
import Network.HTTP.Client                  ( Request )
import Network.HTTP.Types                   (Header, hAuthorization)
import qualified Network.HTTP.Client        as HC

import Qiniu.Types
import Qiniu.HttpClient                     (replaceReqHttpHeader, requestBodyToBsBuilder)


sign :: SecretKey -> ByteString -> ByteString
sign = hmac SHA1.hash 64 . unSecretKey

encodedSign :: SecretKey -> ByteString -> ByteString
encodedSign skey = B64U.encode . sign skey

encodedSign' :: SecretKey -> ByteString -> String
encodedSign' skey = C8.unpack . encodedSign skey

newtype UploadToken = UploadToken { unUploadToken :: String }

uploadToken :: SecretKey -> AccessKey -> PutPolicy -> UploadToken
uploadToken skey akey pp =
    UploadToken $ concat
        [ C8.unpack $ unAccessKey akey
        , ":"
        , encoded_sign
        , ":"
        , C8.unpack $ encoded_pp
        ]
    where
        encoded_pp = B64U.encode $ LB.toStrict $ A.encode pp
        encoded_sign = encodedSign' skey encoded_pp

newtype DownloadToken = DownloadToken { unDownloadToken :: String }

authedDownloadUrl ::
    SecretKey -> AccessKey
    -> UTCTime
    -> String
    -> String
authedDownloadUrl skey akey expiry url =
    url2 ++ "&token=" ++ unDownloadToken token
    where
        encoded_sign    = encodedSign' skey $ UTF8.fromString url2

        e               = if '?' `elem` url
                            then "&e="
                            else "?e="

        url2            = concat
                            [ url
                            , e
                            , show (round $ utcTimeToPOSIXSeconds expiry :: Int64)
                            ]

        token           = DownloadToken $ concat
                            [ C8.unpack $ unAccessKey akey
                            , ":"
                            , encoded_sign
                            ]


-- | 创建 Access Token 的算法
mkAccessToken ::
    SecretKey
    -> AccessKey
    -> ByteString       -- ^ url path
    -> ByteString       -- ^ query string
    -> LB.ByteString    -- ^ body bytestring
    -> AccessToken
mkAccessToken secret_key access_key path qs body_bs =
    AccessToken $ mconcat
        [ unAccessKey access_key
        , ":"
        , encodedSign secret_key $ LB.toStrict sign_str
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


-- | 根据已知的 Http Request 计算 Access Token
mkAccessTokenFromReq ::
    SecretKey
    -> AccessKey
    -> Request
    -> IO AccessToken
mkAccessTokenFromReq secret_key access_key req = do
    body_bs <- requestBodyToBsBuilder body
    return $ mkAccessToken
                secret_key access_key
                path qs
                (BB.toLazyByteString body_bs)
    where
        body    = HC.requestBody req
        path    = HC.path req
        qs      = HC.queryString req


accessTokenHeader :: AccessToken -> Header
accessTokenHeader at = (hAuthorization, "QBox " <> unAccessToken at)


-- | 在 Http Request 里加入 Access Token 相关的头部信息
applyAccessTokenForReq ::
    SecretKey
    -> AccessKey
    -> Request
    -> IO Request
applyAccessTokenForReq secret_key access_key req = do
    token <- mkAccessTokenFromReq secret_key access_key req
    return $ replaceReqHttpHeader (accessTokenHeader token) req
