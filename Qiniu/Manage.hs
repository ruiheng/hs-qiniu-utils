{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Qiniu.Manage where

import Prelude
import qualified Data.Aeson.TH              as AT
import qualified Data.ByteString.Base64.URL as B64U

import Data.Text.Encoding                   (encodeUtf8)
import Data.Int                             (Int64)
import Data.ByteString                      (ByteString)
import Data.Default                         (def)
import Data.Monoid                          ((<>))
-- import Control.Monad.Logger                 (MonadLogger, logDebugS, logInfoS)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Trans.Except           (runExceptT, ExceptT(..))
import Control.Monad.Reader.Class           (MonadReader, ask)
import Control.Monad.Catch                  (MonadCatch, try, throwM)
import Data.String                          (IsString, fromString)
import Network.HTTP.Client                  ( httpLbs, Request, Manager, host, path
                                            , urlEncodedBody, setQueryString
                                            )
import Data.Conduit                         (Source, yield)
import Data.Maybe                           (fromMaybe)

import Qiniu.Utils                          ( lowerFirst
                                            , ServerTimeStamp(..), UrlSafeEncoded(..)
                                            )
import Data.Text                            (Text)

import Qiniu.Types
import Qiniu.Security
import Qiniu.WS.Types

-- | 缺省的接口服务器。
-- 注意，不和为何，不是所有接口都使用相同的接口服务器。
manageApiHost :: IsString a => a
manageApiHost = "rs.qiniu.com"

manageApiReqGet :: ByteString -> Request
manageApiReqGet uri_path =
    def { host = manageApiHost
        , path = uri_path
        }

manageApiReqPost :: [(ByteString, ByteString)] -> ByteString -> Request
manageApiReqPost post_params uri_path =
    urlEncodedBody post_params $ manageApiReqGet uri_path


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


delete :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Entry -> m (WsResult ())
delete secret_key access_key entry = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/delete/" <> encodedEntryUri entry
        req         = manageApiReqPost [] url_path


copy :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Entry        -- ^ from
    -> Entry        -- ^ to
    -> m (WsResult ())
copy secret_key access_key entry_from entry_to = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/copy/" <> encodedEntryUri entry_from
                                <> "/" <> encodedEntryUri entry_to
        req         = manageApiReqPost [] url_path


move :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Entry        -- ^ from
    -> Entry        -- ^ to
    -> m (WsResult ())
move secret_key access_key entry_from entry_to = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/move/" <> encodedEntryUri entry_from
                                <> "/" <> encodedEntryUri entry_to
        req         = manageApiReqPost [] url_path


chgm :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Entry
    -> ByteString
    -> m (WsResult ())
chgm secret_key access_key entry mime = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/chgm/" <> encodedEntryUri entry
                                <> "/mime/" <> B64U.encode mime
        req         = manageApiReqPost [] url_path


data ListItem = ListItem {
                    liKey           :: String
                    , liPutTime     :: ServerTimeStamp
                    , liHash        :: UrlSafeEncoded
                    , liFsize       :: Int64
                    , liMimeType    :: String
                    , liCustomer    :: Maybe String
                }
                deriving (Show)
$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = lowerFirst . drop 2}
    ''ListItem)

data ListResult = ListResult {
                    lrMarker                :: Maybe String
                    , lrCommonPrefixes      :: Maybe [String]
                    , lrItems               :: [ListItem]
                    }
                    deriving (Show)
$(AT.deriveJSON
    AT.defaultOptions{AT.fieldLabelModifier = lowerFirst . drop 2}
    ''ListResult)

list :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Bucket
    -> Int          -- ^ limit
    -> String       -- ^ delimiter
    -> String       -- ^ prefix
    -> String       -- ^ marker
    -> m (WsResult ListResult)
list secret_key access_key bucket limit delimiter prefix marker =
    runExceptT $ do
        mgmt <- ask
        req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
        asWsResponseNormal' =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/list"
        req         = setQueryString
                        [ ("bucket",    Just (fromString $ unBucket bucket))
                        , ("limit",     Just (fromString $ show $
                                                    min 1000 $ max 1 limit))
                        , ("prefix",    Just (fromString prefix))
                        , ("delimiter", Just (fromString delimiter))
                        , ("marker",    Just (fromString marker))
                        ]
                        $ (manageApiReqPost [] url_path) { host = "rsf.qbox.me" }


listSource :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Bucket
    -> Int          -- ^ limit
    -> String       -- ^ delimiter
    -> String       -- ^ prefix
    -> Source m ListResult
                    -- ^ may throw HttpException or WsError
listSource secret_key access_key bucket limit delimiter prefix = do
    go ""
    where
        go marker = do
            lr <- list secret_key access_key bucket limit delimiter prefix marker
                    >>= either (either throwM throwM) return . packError
            yield lr
            let new_marker = fromMaybe "" $ lrMarker lr
            if null new_marker
                then return ()
                else go new_marker


fetch :: (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey
    -> AccessKey
    -> Text         -- ^ from url
    -> Entry        -- ^ to
    -> m (WsResult ())
fetch secret_key access_key url_from entry_to = runExceptT $ do
    mgmt <- ask
    req' <- liftIO $ applyAccessTokenForReq secret_key access_key req
    asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ httpLbs req' mgmt)
    where
        url_path    = "/fetch/" <> B64U.encode (encodeUtf8 url_from)
                                <> "/to/" <> encodedEntryUri entry_to
        req         = (manageApiReqPost [] url_path) { host = "iovip.qbox.me" }


