module Qiniu.Security where

import Prelude
import Data.ByteString                      (ByteString)
import qualified Crypto.Hash.SHA1           as SHA1
import Crypto.MAC.HMAC                      (hmac)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Char8      as C8
import qualified Data.Aeson                 as A
import qualified Data.ByteString.UTF8       as UTF8
import Data.Time.Clock.POSIX                (utcTimeToPOSIXSeconds)
import Data.Time                            (UTCTime)
import Data.Int                             (Int64)

import Qiniu.Types


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
