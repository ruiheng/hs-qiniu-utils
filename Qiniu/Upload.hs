{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Upload where

import Prelude
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import Control.Applicative                  ((<$>))
import Data.Maybe                           (catMaybes)
import Data.String                          (fromString)
import Data.ByteString                      (ByteString)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad                        (liftM)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Catch                  (MonadThrow, try)

import Network.Wreq
import Control.Lens

-- import Qiniu.Security
import Qiniu.Types
import Qiniu.WS.Types

uploadOneShort ::
    (MonadIO m, MonadThrow m) =>
    String
    -> Maybe ResourceKey
    -> Maybe FilePath   -- ^ optionally reveal the local file path
    -> LB.ByteString    -- ^ content of the file to uploaded
    -> m (WsResult (ByteString, ResourceKey))
uploadOneShort upload_token m_key m_fp bs = runExceptT $ do
    let getr = liftIO $ try $ post "http://upload.qiniu.com/" $ catMaybes $
            [ Just $ partText "token" (T.pack upload_token)
            , Just $ partLBS "file" bs & partFileName .~ m_fp
            , (partString "key" . unResourceKey ) <$> m_key
            ]
    rb <- ExceptT getr
    runExceptT $ do
        r <- ExceptT $ asWsResponseNormal rb
        hash <- liftM fromString $ respJsonGetByKey r "hash"
        key <- liftM ResourceKey $ respJsonGetByKey r "key"
        return (hash, key)
