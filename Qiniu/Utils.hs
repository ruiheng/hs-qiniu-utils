module Qiniu.Utils where

-- {{{1 imports
import ClassyPrelude
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as C8

import qualified Data.Char                  as Char
import Data.Scientific                      (floatingOrInteger)
import Data.Aeson                           ( withScientific, withText
                                            , FromJSON, parseJSON
                                            , ToJSON, toJSON
                                            , Value
                                            )
import Data.Time.Clock.POSIX                (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
-- }}}1


lowerFirst :: String -> String
-- {{{1
lowerFirst s =
    case s of
        c:cs    -> Char.toLower c : cs
        _       -> s
-- }}}1


-- | 解释 stat 等接口返回的时间戳。Epoch 时间，以 100 纳秒为单位
newtype ServerTimeStamp = ServerTimeStamp { unServerTimeStamp :: UTCTime }
                        deriving (Eq, Ord)

-- {{{1 instances
instance Show ServerTimeStamp where
    show (ServerTimeStamp x) = "ServerTimeStamp:" ++ show x

instance FromJSON ServerTimeStamp where
    parseJSON = withScientific "ServerTimeStamp" $ \sn -> do
        return $ ServerTimeStamp $ posixSecondsToUTCTime $ realToFrac $
            (either id (fromIntegral :: Integer -> Double)$ floatingOrInteger sn)
                / (1000 * 1000 * 10)

instance ToJSON ServerTimeStamp where
    toJSON = (toJSON :: Double -> Value)
                . realToFrac
                . utcTimeToPOSIXSeconds
                . unServerTimeStamp
-- }}}1


newtype UrlSafeEncoded = UrlSafeEncoded { unUrlSafeEncoded :: ByteString }
                        deriving (Eq, Ord)

-- {{{1 instances
instance Show UrlSafeEncoded where
    show (UrlSafeEncoded x) = "UrlSafeEncoded:" ++ (C8.unpack $ B64U.encode x)

instance FromJSON UrlSafeEncoded where
    parseJSON = withText "url-safe-encoded bytestring" $
        either fail (return . UrlSafeEncoded) . B64U.decode . encodeUtf8

instance ToJSON UrlSafeEncoded where
    toJSON = toJSON . C8.unpack . B64U.encode . unUrlSafeEncoded
-- }}}1



-- vim: set foldmethod=marker:
