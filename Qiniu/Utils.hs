module Qiniu.Utils where

-- {{{1 imports
import ClassyPrelude
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8

import qualified Data.Char                  as Char
import Data.Scientific                      (floatingOrInteger)
import Data.Aeson                           ( withScientific
                                            , FromJSON, parseJSON
                                            , ToJSON, toJSON
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

intServerTimeStamp :: ServerTimeStamp -> Int64
intServerTimeStamp = (* (1000 * 1000 * 10))
                      . round
                      . utcTimeToPOSIXSeconds
                      . unServerTimeStamp

-- {{{1 instances
instance Show ServerTimeStamp where
    show (ServerTimeStamp x) = "ServerTimeStamp:" ++ show x

instance FromJSON ServerTimeStamp where
    parseJSON = withScientific "ServerTimeStamp" $ \sn -> do
        return $ ServerTimeStamp $ posixSecondsToUTCTime $ realToFrac $
            (either id (fromIntegral :: Integer -> Double)$ floatingOrInteger sn)
                / (1000 * 1000 * 10)

instance ToJSON ServerTimeStamp where
    toJSON = toJSON . intServerTimeStamp
-- }}}1


base64UrlEncode :: IsString s => ByteString -> s
base64UrlEncode = fromString . C8.unpack . B64U.encode

base64UrlEncodeT :: IsString s => Text -> s
base64UrlEncodeT = base64UrlEncode . encodeUtf8

base64UrlEncodeS :: IsString s => String -> s
base64UrlEncodeS = base64UrlEncode . encodeUtf8 . pack


isNothingOrNull :: MonoFoldable a => Maybe a -> Bool
isNothingOrNull = fromMaybe True . fmap null


maybeNonNull :: MonoFoldable a => Maybe a -> Maybe a
maybeNonNull (Just x) = if null x then Nothing else Just x
maybeNonNull Nothing = Nothing

-- vim: set foldmethod=marker:
