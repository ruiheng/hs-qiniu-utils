module Qiniu.Utils where

-- {{{1 imports
import ClassyPrelude
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8

import qualified Data.Char                  as Char
import Data.Scientific                      (floatingOrInteger)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as A
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



class NumFromDoubleOrInteger a where
  fromDoubleOrInteger :: Either Double Integer -> Maybe a

-- instance Integral a => NumFromDoubleOrInteger a where
instance NumFromDoubleOrInteger Int where
  fromDoubleOrInteger (Left _) = Nothing
  fromDoubleOrInteger (Right i) = Just $ fromIntegral i

instance NumFromDoubleOrInteger Int64 where
  fromDoubleOrInteger (Left _) = Nothing
  fromDoubleOrInteger (Right i) = Just $ fromIntegral i

instance NumFromDoubleOrInteger Integer where
  fromDoubleOrInteger (Left _) = Nothing
  fromDoubleOrInteger (Right i) = Just i


-- instance Fractional a => NumFromDoubleOrInteger a where
instance NumFromDoubleOrInteger Double where
  fromDoubleOrInteger = return . either id fromIntegral

instance NumFromDoubleOrInteger Float where
  fromDoubleOrInteger = return . either realToFrac fromIntegral


-- 魔法变量里经常出现意义是数字，但实际上用字串格式表达的json字段
newtype StrNumber a = StrNumber { unStrNumber :: a }

instance Show a => ToJSON (StrNumber a) where
  toJSON (StrNumber x) = toJSON $ tshow x

instance (NumFromDoubleOrInteger a, Read a) => FromJSON (StrNumber a) where
  parseJSON (A.String x) =
    maybe (fail $ "cannot parse as number: " <> unpack x) (return . StrNumber) $ readMay x

  parseJSON (A.Number x) =
    maybe (fail $ "cannot parse as number: " <> show x) (return . StrNumber) $
      fromDoubleOrInteger (floatingOrInteger x)

  parseJSON x = A.typeMismatch "number" x


nonNullStrNumber :: A.Value -> Bool
nonNullStrNumber (A.String x) = not $ null x
nonNullStrNumber _            = False


-- 不能使用 AvInfo 本身的 FromJSON 实例自动生成 (Maybe AvInfo) 的　FromJSON 实例
-- 因为实测表明当内容不适用时，七牛不是把整个值变成null，而是保留结构，让所有字段填上空白字串，或不明意义的字面值
-- 这个包装让我们可以在FromJSON实例实现时插入一些特别的逻辑，以检查得到的是一个正常的值，还是只是一个占位数据
newtype MaybeMagic a = MaybeMagic { unMaybeMagic :: Maybe a }
  deriving (Show, Functor, Applicative)

instance (NumFromDoubleOrInteger a, Read a) => FromJSON (MaybeMagic (StrNumber a)) where
  parseJSON v@(A.String x) = fmap MaybeMagic $
    if null x
       then return Nothing
       else parseJSON v

  parseJSON A.Null = fmap MaybeMagic $ return Nothing

  parseJSON v = fmap MaybeMagic $ parseJSON v


-- vim: set foldmethod=marker:
