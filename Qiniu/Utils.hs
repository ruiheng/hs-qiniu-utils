module Qiniu.Utils where

-- {{{1 imports
import ClassyPrelude

import qualified Data.Char                  as Char
import Data.Scientific                      (floatingOrInteger)
import Data.Aeson                           ( withScientific
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



-- vim: set foldmethod=marker:
