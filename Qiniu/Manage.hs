{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Qiniu.Manage where

-- {{{1 imports
import           ClassyPrelude hiding (try, delete)
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


type QiniuManageMonad m a = (QiniuRemoteCallMonad m) => ReaderT (SecretKey, AccessKey) m a

data EntryStat =
       EntryStat
         { eStatFsize :: Int64
         , eStatHash :: EtagHash
         , eStatMimeType :: Text
         , eStatPutTime :: ServerTimeStamp
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 5 } ''EntryStat)

stat :: MonadBaseControl IO m
     => Entry
     -> QiniuManageMonad m (WsResult EntryStat)
-- {{{1
stat entry = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path defaults
  resp <- ExceptT $ liftIO $ try $ WS.getWith opts sess url
  asWsResponseNormal' resp `Lifted.onException`
    ($logErrorS logSource $ "Cannot parse response body: " <> toStrict (decodeUtf8 (view responseBody resp)))
  where
    url_path = "/stat/" <> encodedEntryUri entry
    url = manageApiUrl $ C8.unpack url_path
-- }}}1


statMaybe :: MonadBaseControl IO m
          => Entry
          -> QiniuManageMonad m (WsResult (Maybe EntryStat))
statMaybe = fmap maybeDoesNotExist . stat


-- | Test whether an entry already exists and with the same etag
alreadyExistsAndMatch :: MonadBaseControl IO m
                      => Entry
                      -> m (Maybe EtagHash)  -- ^ 已有文件的hash. Nothing 代表不检查Etag
                      -> QiniuManageMonad m (WsResult Bool)
-- {{{1
alreadyExistsAndMatch entry get_local_etag = do
  err_or_st <- fmap packError $ stat entry
  case err_or_st of
    Left err -> if isResourceDoesNotExistError err
                   then return $ Right $ Right False
                   else return $ either Left (Right . Left) err
    Right st -> do
      m_etag <- lift $ get_local_etag
      return $ Right $ Right $
        case m_etag of
          Nothing   -> True
          Just etag -> if etag == eStatHash st
                          then True
                          else False
-- }}}1

-- | Like alreadyExistsAndMatch, but consider any error as 'does not exist'
alreadyExistsAndMatch' :: (MonadBaseControl IO m)
                       => Entry
                       -> m (Maybe EtagHash)  -- ^ 已有文件的hash
                       -> QiniuManageMonad m Bool
-- {{{1
alreadyExistsAndMatch' entry get_local_etag = do
  err_or_exists <- fmap packError $ alreadyExistsAndMatch entry get_local_etag
  case err_or_exists of
    Right x -> return x
    Left err -> do
      $logErrorS logSource $ "alreadyExistsAndMatch failed: " <> tshow err
      return False
-- }}}1


delete :: Entry -> QiniuManageMonad m (WsResult ())
-- {{{1
delete entry = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/delete/" <> encodedEntryUri entry
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


deleteMaybe :: Entry -> QiniuManageMonad m (WsResult (Maybe ()))
deleteMaybe = fmap maybeDoesNotExist . delete


deleteAfterDays :: Entry
                -> Maybe Int  -- ^ Nothing: disable lifecyle
                -> QiniuManageMonad m (WsResult ())
-- {{{1
deleteAfterDays entry m_days = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/deleteAfterDays/" <> encodedEntryUri entry <> "/" <> fromString (show (fromMaybe 0 m_days))
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1

deleteAfterDaysMaybe :: Entry
                     -> Maybe Int  -- ^ Nothing: disable lifecyle
                     -> QiniuManageMonad m (WsResult (Maybe ()))
deleteAfterDaysMaybe = (fmap maybeDoesNotExist .) . deleteAfterDays


changeStoreType :: Entry
                -> FileStoreType
                -> QiniuManageMonad m (WsResult ())
-- {{{1
changeStoreType entry st = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/chtype/" <> encodedEntryUri entry <> "/type/" <> fromString (show (fromEnum st))
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1

changeStoreTypeMaybe :: Entry -> FileStoreType -> QiniuManageMonad m (WsResult (Maybe ()))
changeStoreTypeMaybe = (fmap maybeDoesNotExist .) . changeStoreType


copy :: Entry        -- ^ from
     -> Entry        -- ^ to
     -> QiniuManageMonad m (WsResult ())
-- {{{1
copy entry_from entry_to = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/copy/" <> encodedEntryUri entry_from
               <> "/" <> encodedEntryUri entry_to
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1


copyMaybe :: Entry        -- ^ from
          -> Entry        -- ^ to
          -> QiniuManageMonad m (WsResult (Maybe (Maybe ())))
          -- ^ Nothing: source does not exist
          --   Just Nothing: dest does not exist
          --   Just (Just ()): success
copyMaybe = (fmap (maybeDoesNotExist . maybeAlreadyExists) .) . copy


move :: Entry        -- ^ from
     -> Entry        -- ^ to
     -> QiniuManageMonad m (WsResult ())
-- {{{1
move entry_from entry_to = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/move/" <> encodedEntryUri entry_from
               <> "/" <> encodedEntryUri entry_to
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1

moveMaybe :: Entry        -- ^ from
          -> Entry        -- ^ to
          -> QiniuManageMonad m (WsResult (Maybe (Maybe ())))
          -- ^ Nothing: source does not exist
          --   Just Nothing: dest does not exist
          --   Just (Just ()): success
moveMaybe = (fmap (maybeDoesNotExist . maybeAlreadyExists) .) . move


-- | TODO: 未实现修改 meta_key 及 cond
chgm :: Entry
     -> ByteString
     -> QiniuManageMonad m (WsResult ())
-- {{{1
chgm entry mime = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path post_data defaults
  asWsResponseEmpty =<< (ExceptT $ liftIO $ try $ WS.postWith opts sess url post_data)
  where
    url_path = "/chgm/" <> encodedEntryUri entry
               <> "/mime/" <> B64U.encode mime
    url = manageApiUrl $ C8.unpack url_path
    post_data = mempty :: ByteString
-- }}}1

chgmMaybe :: Entry -> ByteString -> QiniuManageMonad m (WsResult (Maybe ()))
chgmMaybe = (fmap maybeDoesNotExist .) . chgm


data ListItem =
       ListItem
         { liKey :: Text
         , liPutTime :: ServerTimeStamp
         , liHash :: EtagHash
         , liFsize :: Int64
         , liMimeType :: Text
         , liCustomer :: Maybe Text
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListItem)

data ListResult =
       ListResult
         { lrMarker :: Maybe Text
         , lrCommonPrefixes :: Maybe [Text]
         , lrItems :: [ListItem]
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListResult)


list :: Bucket
     -> Int          -- ^ limit
     -> Text       -- ^ delimiter
     -> Text       -- ^ prefix
     -> Text       -- ^ marker
     -> QiniuManageMonad m (WsResult ListResult)
-- {{{1
list bucket limit delimiter prefix marker = do
  sess <- lift ask
  (secret_key, access_key) <- ask
  runExceptT $ do
    let opts0 = defaults
                  & param "bucket" .~ [unBucket bucket]
                  & param "limit" .~ [ tshow $ min 1000 $ max 1 limit ]
                  & param "prefix" .~ [prefix]
                  & param "delimiter" .~ [delimiter]
                  & param "marker" .~ [marker]

    opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path opts0
    asWsResponseNormal' =<< (ExceptT $ liftIO $ try $ WS.getWith opts sess url)
  where
    url_path = "/list"
    url = manageApiUrlF $ C8.unpack url_path
-- }}}1


listSource :: QiniuRemoteCallMonad m
           => Bucket
           -> Int          -- ^ limit
           -> Text       -- ^ delimiter
           -> Text       -- ^ prefix
           -> Source (ReaderT (SecretKey, AccessKey) m) ListResult
            -- ^ may throw HttpException or WsError
-- {{{1
listSource bucket limit delimiter prefix = do
  go ""

  where
    go marker = do
      lr <- lift (list bucket limit delimiter prefix marker)
            >>= either (either throwM throwM) return . packError
      yield lr
      let new_marker = fromMaybe "" $ lrMarker lr
      if null new_marker
        then return ()
        else go new_marker
-- }}}1


fetch :: Text         -- ^ from url
      -> Scope        -- ^ to
      -> QiniuManageMonad m (WsResult UploadedFileInfo)
-- {{{1
fetch url_from scope_to = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenGet secret_key access_key url_path defaults
  asWsResponseNormal' =<< (ExceptT $ liftIO $ try $ WS.getWith opts sess url)
  where
    url_path = "/fetch/" <> B64U.encode (encodeUtf8 url_from)
               <> "/to/" <> encodedScopeUri scope_to
    url = "http://iovip.qbox.me" <> C8.unpack url_path
-- }}}1



-- vim: set foldmethod=marker:
