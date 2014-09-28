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
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Network.HTTP.Client                  (HttpException)

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
type WsResult a = Either HttpException (Either WsError a)
