{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qiniu.Manage where

-- {{{1 imports
import           ClassyPrelude hiding (try)
import qualified Control.Exception.Lifted as Lifted
import           Control.Lens (view, (&), (.~))
import           Control.Monad.Logger
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8

-- import Control.Monad.Logger                 (MonadLogger, logDebugS, logInfoS)
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import           Control.Monad.Catch (try)
import           Network.Wreq (responseBody, defaults, param)
import qualified Network.Wreq.Session as WS
import           Data.Conduit (Source, yield)

import           Qiniu.Utils (lowerFirst, ServerTimeStamp(..))

import           Qiniu.Types
import           Qiniu.Security
import           Qiniu.WS.Types
import           Qiniu.Error
-- }}}1


-- | 缺省的接口服务器。 注意，不和为何，不是所有接口都使用相同的接口服务器。
manageApiHost :: IsString a => a
manageApiHost = "rs.qiniu.com"

manageApiHostF :: IsString a => a
manageApiHostF = "rsf.qbox.me"

manageApiUrl :: String -> String
manageApiUrl p = "http://" <> manageApiHost <> p

manageApiUrlF :: String -> String
manageApiUrlF p = "http://" <> manageApiHostF <> p



data EntryStat =
       EntryStat
         { eStatFsize :: Int64
         , eStatHash :: EtagHash
         , eStatMimeType :: String
         , eStatPutTime :: ServerTimeStamp
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 5 } ''EntryStat)

stat :: (MonadIO m, MonadReader WS.Session m, MonadCatch m, MonadLogger m, MonadBaseControl IO m)
     => SecretKey
     -> AccessKey
     -> Entry
     -> m (WsResult EntryStat)
-- {{{1
stat secret_key access_key entry = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path defaults
  resp <- ExceptT $ try $ liftIO $ WS.getWith opts sess url
  asWsResponseNormal' resp `Lifted.onException`
    ($logErrorS logSource $ "Cannot parse response body: " <> toStrict (decodeUtf8 (view responseBody resp)))
  where
    url_path = "/stat/" <> encodedEntryUri entry
    url = manageApiUrl $ C8.unpack url_path
-- }}}1

statMaybe :: (MonadIO m, MonadReader WS.Session m, MonadCatch m, MonadLogger m, MonadBaseControl IO m)
          => SecretKey
          -> AccessKey
          -> Entry
          -> m (WsResult (Maybe EntryStat))
-- {{{1
statMaybe secret_key access_key entry = do
  fmap maybeDoesNotExists $ stat secret_key access_key entry
-- }}}1


-- | Test whether an entry already exists and with the same etag
alreadyExistsAndMatch :: (MonadIO m, MonadReader WS.Session m, MonadCatch m, MonadLogger m, MonadBaseControl IO m)
                      => SecretKey
                      -> AccessKey
                      -> Entry
                      -> m (Maybe EtagHash)  -- ^ 已有文件的hash. Nothing 代表不检查Etag
                      -> m (WsResult Bool)
-- {{{1
alreadyExistsAndMatch secret_key access_key entry get_local_etag = do
  err_or_st <- fmap packError $ stat secret_key access_key entry
  case err_or_st of
    Left err -> if isResourceDoesNotExistError err
                   then return $ Right $ Right False
                   else return $ either Left (Right . Left) err
    Right st -> do
      m_etag <- get_local_etag
      return $ Right $ Right $
        case m_etag of
          Nothing   -> True
          Just etag -> if etag == eStatHash st
                          then True
                          else False
-- }}}1

-- | Like alreadyExistsAndMatch, but consider any error as 'does not exist'
alreadyExistsAndMatch' :: (MonadIO m, MonadReader WS.Session m, MonadCatch m, MonadLogger m, MonadLogger m, MonadBaseControl IO m)
                       => SecretKey
                       -> AccessKey
                       -> Entry
                       -> m (Maybe EtagHash)  -- ^ 已有文件的hash
                       -> m Bool
-- {{{1
alreadyExistsAndMatch' secret_key access_key entry get_local_etag = do
  err_or_exists <- fmap packError $ alreadyExistsAndMatch secret_key access_key entry get_local_etag
  case err_or_exists of
    Right x -> return x
    Left err -> do
      $logErrorS logSource $ "alreadyExistsAndMatch failed: " <> tshow err
      return False
-- }}}1


delete :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
       => SecretKey
       -> AccessKey
       -> Entry -> m (WsResult ())
-- {{{1
delete secret_key access_key entry = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/delete/" <> encodedEntryUri entry
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


deleteAfterDays :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
                => SecretKey
                -> AccessKey
                -> Entry
                -> Maybe Int  -- ^ Nothing: disable lifecyle
                -> m (WsResult ())
-- {{{1
deleteAfterDays secret_key access_key entry m_days = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/deleteAfterDays/" <> encodedEntryUri entry <> "/" <> fromString (show (fromMaybe 0 m_days))
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


changeStoreType :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
                => SecretKey
                -> AccessKey
                -> Entry
                -> FileStoreType
                -> m (WsResult ())
-- {{{1
changeStoreType secret_key access_key entry st = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/chtype/" <> encodedEntryUri entry <> "/type/" <> fromString (show (fromEnum st))
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


copy :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
     => SecretKey
     -> AccessKey
     -> Entry        -- ^ from
     -> Entry        -- ^ to
     -> m (WsResult ())
-- {{{1
copy secret_key access_key entry_from entry_to = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/copy/" <> encodedEntryUri entry_from
               <> "/" <> encodedEntryUri entry_to
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


move :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
     => SecretKey
     -> AccessKey
     -> Entry        -- ^ from
     -> Entry        -- ^ to
     -> m (WsResult ())
-- {{{1
move secret_key access_key entry_from entry_to = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/move/" <> encodedEntryUri entry_from
               <> "/" <> encodedEntryUri entry_to
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


-- | TODO: 未实现修改 meta_key 及 cond
chgm :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
     => SecretKey
     -> AccessKey
     -> Entry
     -> ByteString
     -> m (WsResult ())
-- {{{1
chgm secret_key access_key entry mime = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ try $ liftIO $ WS.postWith opts sess url post_data)
  where
    url_path = "/chgm/" <> encodedEntryUri entry
               <> "/mime/" <> B64U.encode mime
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


data ListItem =
       ListItem
         { liKey :: String
         , liPutTime :: ServerTimeStamp
         , liHash :: EtagHash
         , liFsize :: Int64
         , liMimeType :: String
         , liCustomer :: Maybe String
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListItem)

data ListResult =
       ListResult
         { lrMarker :: Maybe String
         , lrCommonPrefixes :: Maybe [String]
         , lrItems :: [ListItem]
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListResult)


list :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
     => SecretKey
     -> AccessKey
     -> Bucket
     -> Int          -- ^ limit
     -> String       -- ^ delimiter
     -> String       -- ^ prefix
     -> String       -- ^ marker
     -> m (WsResult ListResult)
-- {{{1
list secret_key access_key bucket limit delimiter prefix marker =
  runExceptT $ do
    let opts0 = defaults
                  & param "bucket" .~ [fromString $ unBucket bucket]
                  & param "limit" .~ [ tshow $ min 1000 $ max 1 limit ]
                  & param "prefix" .~ [fromString prefix]
                  & param "delimiter" .~ [fromString delimiter]
                  & param "marker" .~ [fromString marker]

    sess <- ask
    opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path opts0
    asWsResponseNormal' =<< (ExceptT $ try $ liftIO $ WS.getWith opts sess url)
  where
    url_path = "/list"
    url = manageApiUrlF $ C8.unpack url_path
-- }}}1


listSource :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
           => SecretKey
           -> AccessKey
           -> Bucket
           -> Int          -- ^ limit
           -> String       -- ^ delimiter
           -> String       -- ^ prefix
           -> Source m ListResult
            -- ^ may throw HttpException or WsError
-- {{{1
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
-- }}}1


fetch :: (MonadIO m, MonadReader WS.Session m, MonadCatch m)
      => SecretKey
      -> AccessKey
      -> Text         -- ^ from url
      -> Scope        -- ^ to
      -> m (WsResult UploadedFileInfo)
-- {{{1
fetch secret_key access_key url_from scope_to = runExceptT $ do
  sess <- ask
  opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path defaults
  asWsResponseNormal' =<< (ExceptT $ try $ liftIO $ WS.getWith opts sess url)
  where
    url_path = "/fetch/" <> B64U.encode (encodeUtf8 url_from)
               <> "/to/" <> encodedScopeUri scope_to
    url = "http://iovip.qbox.me" <> C8.unpack url_path
-- }}}1



-- vim: set foldmethod=marker:
