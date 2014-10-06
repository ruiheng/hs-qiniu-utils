{-# LANGUAGE OverloadedStrings #-}
module Qiniu.WS.Types where

import Prelude
import qualified Data.ByteString.Lazy       as LB
import Data.Map                             (Map)
import qualified Data.Map                   as Map
import Data.Aeson                           (Value, withObject, (.:)
                                            , FromJSON, parseJSON)
import qualified Data.Aeson                 as A
import Control.Monad.Catch                  (MonadThrow, throwM)
import Control.Applicative                  ((<$>), (<*>), (<|>))
import Control.Monad                        (liftM)
import Control.Monad.Trans.Class            (MonadTrans, lift)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Network.HTTP.Client                  (HttpException(..))

import Network.Wreq
import Control.Lens

type WsRespBodyNormal = Map String Value

data WsError = WsError {
                    wsHttpCode :: Int
                    , wsErrMsg :: String
                }
                deriving (Eq, Show)


instance FromJSON WsError where
    parseJSON = withObject "WsError" $ \obj -> do
                    WsError <$> (obj .: "code")
                            <*> (obj .: "error")


newtype WsRespBody = WsRespBody { unWsRespBody :: Either WsError WsRespBodyNormal }
                    deriving (Eq, Show)

instance FromJSON WsRespBody where
    parseJSON v = do
        fmap WsRespBody $ (Left <$> parseJSON v) <|> (Right <$> parseJSON v)

type WsResponse = Response WsRespBody

asWsResponse :: (MonadThrow m) => Response LB.ByteString -> m WsResponse
asWsResponse = asJSON


asWsResponseNormal :: (MonadThrow m) =>
    Response LB.ByteString
    -> m (Either WsError (Response WsRespBodyNormal))
asWsResponseNormal rb = do
    r <- liftM (over responseBody unWsRespBody) $ asWsResponse rb
    return $ case r ^. responseBody of
        Left err -> Left err
        Right nb -> Right $ r & responseBody .~ nb

asWsResponseNormal' :: (MonadThrow m, FromJSON a) =>
    Response LB.ByteString
    -> m (Either WsError a)
asWsResponseNormal' rb = runExceptT $ do
    r <- ExceptT $ asWsResponseNormal rb
    case A.fromJSON $ A.toJSON $ r ^. responseBody of
        A.Error err -> throwM $ JSONError err
        A.Success x -> return x

respJsonGetByKey :: (MonadThrow m, FromJSON a) =>
    Response WsRespBodyNormal
    -> String
    -> m a
respJsonGetByKey r k = do
    v <- maybe
        (throwM $ JSONError $ "missing key in JSON object: " ++ k)
        return
        (Map.lookup k $ r ^. responseBody)
    case A.fromJSON v of
        A.Error err -> throwM $ JSONError $
                        "failed to parse value of key '" ++ k
                            ++ "' in JSON object: " ++ err
        A.Success x -> return x

-- | 服务器可能在 HTTP 层上出错，也可能在 API 报文内报错
-- 这个类型方便分层处理错误
type WsResult a = Either HttpException (Either WsError a)

-- | 这个类型方便统一处理两种错误
type WsResultP a = Either (Either HttpException WsError) a

packError :: WsResult a -> WsResultP a
packError (Left x)          = Left $ Left x
packError (Right (Left x))  = Left $ Right x
packError (Right (Right x)) = Right x

unpackError :: WsResultP a -> WsResult a
unpackError (Left (Left x))     = Left x
unpackError (Left (Right x))    = Right (Left x)
unpackError (Right x)           = Right (Right x)


-- | 根据 retryWsCall 的实现，这种函数不但实现错误报告
-- 还可以实现错误时重试的时间间隔（直接 threadDelay）
-- 返回值表示是否可以重试
type OnWsCallError m = String -> Int -> Either HttpException WsError -> m Bool

nopOnWsCallError :: Monad m => Int -> OnWsCallError m
nopOnWsCallError max_try _ call_cnt _ = return $ max_try > call_cnt

-- | 这个函数预期行为是：决定远程调用是否可以重试
-- 但现在没有足够的信息决定哪些错误情况适合重试
-- 要根据以后调试过程的经验完善
resumableError :: Int -> Either HttpException WsError -> Bool
resumableError _ (Left ResponseTimeout)                 = True
resumableError _ (Left (FailedConnectionException {}))  = True
resumableError _ (Left (FailedConnectionException2 {})) = True
resumableError _ _                                      = False

-- | used with retryWhile
shouldRetryWsCall :: Monad m =>
    (Int -> Either HttpException WsError -> m Bool)
    -> Int
    -> WsResultP a
    -> m Bool
shouldRetryWsCall on_err call_cnt result = do
    case result of
        Right _ -> return False
        Left err -> do
                    b <- on_err call_cnt err
                    return $ b && resumableError call_cnt err

retryWsCall :: Monad m =>
    String                   -- ^ context: error function and the like
    -> OnWsCallError m
    -> m (WsResultP a)
    -> m (WsResultP a)
retryWsCall func on_err = retryWhile (shouldRetryWsCall $ on_err func)

----------------------------------------------------------

retryWhile :: Monad m => (Int -> a -> m Bool) -> m a -> m a
retryWhile p f = go 1
    where
        go cnt = do
            x <- f
            b <- p cnt x
            if b
                then go (cnt + 1)
                else return x

lift3 :: (MonadTrans t, Monad m) =>
    (a -> b -> c -> m d)
    -> (a -> b -> c -> t m d)
lift3 f x y z = lift $ f x y z

lift2 :: (MonadTrans t, Monad m) =>
    (a -> b -> m c)
    -> (a -> b -> t m c)
lift2 f x y = lift $ f x y

lift1 :: (MonadTrans t, Monad m) =>
    (a -> m b)
    -> (a -> t m b)
lift1 f x = lift $ f x

