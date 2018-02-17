{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Qiniu.WS.Types where

-- {{{1 imports
import ClassyPrelude hiding (catch)
import qualified Data.ByteString.Lazy       as LB
import Data.Aeson                           (Value, withObject, (.:)
                                            , FromJSON, parseJSON)
import qualified Data.Aeson                 as A
import Control.Monad.Catch                  (catch)
import Control.Monad.Trans.Class            (MonadTrans)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Network.HTTP.Client                  (HttpException(..))
#if MIN_VERSION_http_client(0, 5, 0)
import Network.HTTP.Client                  (HttpExceptionContent(..), responseStatus)
#endif
import Network.HTTP.Types                   (statusCode)

import Network.Wreq hiding (statusCode)
import qualified Network.Wreq as Wreq
import Control.Lens
-- }}}1


type WsRespBodyNormal = Map Text Value

-- | 现在发现错误代码可能不会在json结构里出现
-- 但又不知道是否所有接口都这样
-- 所以做了这样一个hack: json结构里没有错误代码的，先使用某个不可能的值占位
-- 然后再从http状态里取状态码
wsErrorCodePlaceHolder :: Int
wsErrorCodePlaceHolder = -1

data WsError =
       WsError
         { __wsHttpCode :: Int
         -- ^ CAUTION: 见下面的注释： 实测看，七牛可能尽量在 http status 中返回错误代码 而不是 json 结构里
         , wsErrMsg :: Text
         }
  deriving (Eq, Show, Typeable)

-- {{{1 instances
instance Exception WsError

instance FromJSON WsError where
    parseJSON = withObject "WsError" $ \obj -> do
      WsError <$> (obj A..:? "code" A..!= wsErrorCodePlaceHolder)
              <*> (obj .: "error")
-- }}}1


newtype WsRespBody = WsRespBody { unWsRespBody :: Either WsError WsRespBodyNormal }
                    deriving (Eq, Show)

instance FromJSON WsRespBody where
    parseJSON v = do
        fmap WsRespBody $ (Left <$> parseJSON v) <|> (Right <$> parseJSON v)

type WsResponse = Response WsRespBody

asWsResponse :: (MonadThrow m) => Response LB.ByteString -> m WsResponse
asWsResponse = asJSON


asWsResponseEmpty :: (MonadThrow m)
                  => Response LB.ByteString
                  -> m (Either WsError ())
-- {{{1
asWsResponseEmpty rb = runExceptT $ do
    if LB.null (rb ^. responseBody)
        then return ()
        else do
            _ <- ExceptT $ asWsResponseNormal rb
            -- 本预期没有任何响应，但实际上又有，暂时忽略它
            return ()
-- }}}1

asWsResponseNormal :: (MonadThrow m)
                   => Response LB.ByteString
                   -> m (Either WsError (Response WsRespBodyNormal))
-- {{{1
asWsResponseNormal rb = do
  r <- liftM (over responseBody unWsRespBody) $ asWsResponse rb
  case r ^. responseBody of
    Left err -> do err' <- if __wsHttpCode err /= wsErrorCodePlaceHolder
                              then return err
                              else do let status_code = statusCode $ r ^. Wreq.responseStatus
                                      return $ err { __wsHttpCode = status_code }
                   return $ Left err'

    Right nb -> return $ Right $ r & responseBody .~ nb
-- }}}1

asWsResponseNormal' :: (MonadThrow m, FromJSON a)
                    => Response LB.ByteString
                    -> m (Either WsError a)
-- {{{1
asWsResponseNormal' rb = runExceptT $ do
    r <- ExceptT $ asWsResponseNormal rb
    case A.fromJSON $ A.toJSON $ r ^. responseBody of
        A.Error err -> throwM $ JSONError err
        A.Success x -> return x
-- }}}1

respJsonGetByKey :: (MonadThrow m, FromJSON a)
                 => Response WsRespBodyNormal
                 -> Text
                 -> m a
-- {{{1
respJsonGetByKey r k = do
    v <- maybe
        (throwM $ JSONError $ unpack $ "missing key in JSON object: " <> k)
        return
        (lookup k $ r ^. responseBody)
    case A.fromJSON v of
        A.Error err -> throwM $ JSONError $ unpack $
                        "failed to parse value of key '" <> k
                            <> "' in JSON object: " <> fromString err
        A.Success x -> return x
-- }}}1

-- | 服务器可能在 HTTP 层上出错，也可能在 API 报文内报错
-- 这个类型方便分层处理错误
type WsResult a = Either HttpException (Either WsError a)

-- | 这个类型方便统一处理两种错误
type WsResultP a = Either (Either HttpException WsError) a


-- | 文档把所有错误代码叫 http 状态码
-- 但有些接口文档描述的返回 json 格式中又有一个 code 字段，也叫 http code
-- 不是非常确定实际上每个接口的错误代码到底是从 http 协议里的 status 返回，
-- 还是从 json 结构里返回。
-- 比如上传文件：实测发现，如果文件已存在，则出 614 错，是从 http 头里返回的
-- 但文档应该从 json 结构里返回
-- 我们代码尽量兼容这两种可能
-- 这个函数的责任就是从两个地方提取提取错误代码
wsErrorCode :: Either HttpException WsError
            -> Maybe Int
-- {{{1
#if MIN_VERSION_http_client(0, 5, 0)
wsErrorCode (Left (InvalidUrlException {}))                              = Nothing
wsErrorCode (Left (HttpExceptionRequest _ (StatusCodeException resp _))) = Just $ statusCode $ Network.HTTP.Client.responseStatus resp
wsErrorCode (Left _)                                                     = Nothing
#else
wsErrorCode (Left (StatusCodeException status _ _)) = Just $ statusCode status
wsErrorCode (Left _)                                = Nothing
#endif
wsErrorCode (Right e)                               = Just $ __wsHttpCode e
-- }}}1


testWsErrorCode :: (Int -> Bool)
                -> Either HttpException WsError
                -> Bool
testWsErrorCode f e = fromMaybe False $ wsErrorCode e >>= return . f


packError :: WsResult a -> WsResultP a
-- {{{1
packError (Left x)          = Left $ Left x
packError (Right (Left x))  = Left $ Right x
packError (Right (Right x)) = Right x
-- }}}1


unpackError :: WsResultP a -> WsResult a
-- {{{1
unpackError (Left (Left x))     = Left x
unpackError (Left (Right x))    = Right (Left x)
unpackError (Right x)           = Right (Right x)
-- }}}1


tryWsResult :: MonadCatch m => m a -> m (WsResultP a)
-- {{{1
tryWsResult f = do
    liftM Right f `catch` h1 `catch` h2
    where
        h1 = return . Left . Left
        h2 = return . Left . Right
-- }}}1


-- | 根据 retryWsCall 的实现，这种函数不但实现错误报告
-- 还可以实现错误时重试的时间间隔（直接 threadDelay）
-- 返回值表示是否可以重试
type OnWsCallError m = Text -> Int -> Either HttpException WsError -> m Bool

nopOnWsCallError :: Monad m => Int -> OnWsCallError m
nopOnWsCallError max_try _ call_cnt _ = return $ max_try > call_cnt

-- | 这个函数预期行为是：决定远程调用是否可以重试
-- 但现在没有足够的信息决定哪些错误情况适合重试
-- 要根据以后调试过程的经验完善
resumableError :: Int -> Either HttpException WsError -> Bool
-- {{{1
#if MIN_VERSION_http_client(0, 5, 0)
resumableError _ (Left (HttpExceptionRequest _ (ResponseTimeout)))                = True
resumableError _ (Left (HttpExceptionRequest _ (ConnectionFailure {})))  = True
resumableError _ (Left (HttpExceptionRequest _ (ConnectionClosed {}))) = True
#else
resumableError _ (Left ResponseTimeout)                 = True
resumableError _ (Left (FailedConnectionException {}))  = True
resumableError _ (Left (FailedConnectionException2 {})) = True
#endif
resumableError _ _                                      = False
-- }}}1

-- | used with retryWhile
shouldRetryWsCall :: Monad m
                  => (Int -> Either HttpException WsError -> m Bool)
                  -> Int
                  -> WsResultP a
                  -> m Bool
-- {{{1
shouldRetryWsCall on_err call_cnt result = do
    case result of
        Right _ -> return False
        Left err -> do
                    b <- on_err call_cnt err
                    return $ b && resumableError call_cnt err
-- }}}1

retryWsCall :: Monad m
            => Text                   -- ^ context: error function and the like
            -> OnWsCallError m
            -> m (WsResultP a)
            -> m (WsResultP a)
retryWsCall func on_err = retryWhile (shouldRetryWsCall $ on_err func)

----------------------------------------------------------

retryWhile :: Monad m => (Int -> a -> m Bool) -> m a -> m a
-- {{{1
retryWhile p f = go 1
    where
        go cnt = do
            x <- f
            b <- p cnt x
            if b
                then go (cnt + 1)
                else return x
-- }}}1

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




-- vim: set foldmethod=marker:
