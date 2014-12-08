{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Qiniu.Manage where

import Prelude
import qualified Data.Aeson.TH              as AT

import Data.Int                             (Int64)
import Data.ByteString                      (ByteString)
import Data.Default                         (def)
import Data.Monoid                          ((<>))
-- import Control.Monad.Logger                 (MonadLogger, logDebugS, logInfoS)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad.Reader.Class           (MonadReader, ask)
import Control.Monad.Catch                  (MonadCatch, try)
import Data.String                          (IsString)
import Network.HTTP.Client                  (httpLbs, Request, Manager, host, path)

import Qiniu.Utils                          ( lowerFirst
                                            , ServerTimeStamp(..), UrlSafeEncoded(..)
                                            )
import Qiniu.Types
import Qiniu.Security
import Qiniu.WS.Types

manageApiHost :: IsString a => a
manageApiHost = "rs.qiniu.com"

manageApiReqGet :: ByteString -> Request
manageApiReqGet uri_path =
    def { host = manageApiHost
        , path = uri_path
        }


data EntryStat = EntryStat {
                    eStatFsize          :: Int64
                    , eStatHash         :: UrlSafeEncoded
                    , eStatMimeType     :: String
                    , eStatPutTime      :: ServerTimeStamp
                }
                deriving (Show)

$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = lowerFirst . drop 5}
    ''EntryStat)


stat :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Entry -> m (WsResult EntryStat)
stat secret_key access_key entry = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseNormal' =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/stat/" <> encodedEntryUri entry
        req         = manageApiReqGet url_path
