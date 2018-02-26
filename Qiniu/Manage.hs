{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Qiniu.Manage where

-- {{{1 imports
import           ClassyPrelude hiding (try, delete)
import qualified Control.Exception.Lifted as Lifted
import           Control.Lens (view, (&), (.~))
import           Control.Monad.Logger
import qualified Data.Aeson.TH as AT
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8
import           Network.HTTP.Types (renderQueryText)

-- import Control.Monad.Logger                 (MonadLogger, logDebugS, logInfoS)
import           Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import           Control.Monad.Catch (try)
import           Network.Mime (MimeType)
import           Network.Wreq (responseBody, defaults, param, Options)
import           Network.Wreq.Types (Postable)
import qualified Network.Wreq.Session as WS
import           Data.Conduit (Source, yield)

import           Qiniu.Utils (lowerFirst, ServerTimeStamp(..), intServerTimeStamp)

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

manageApiHostApi :: IsString a => a
manageApiHostApi = "api.qiniu.com"

manageApiUrl :: String -> String
manageApiUrl p = "http://" <> manageApiHost <> p

manageApiUrlF :: String -> String
manageApiUrlF p = "http://" <> manageApiHostF <> p

manageApiUrlApi :: String -> String
manageApiUrlApi p = "http://" <> manageApiHostApi <> p


type QiniuManageMonad m a = (QiniuRemoteCallMonad m) => ReaderT (SecretKey, AccessKey) m a


-- | 描述不同管理操作指令的实现区别
-- 每一增加一新的操作，需增加一种类型，并实现此 class
class ( AsWsResponse (ObjectManageOpResult a)
      , Postable (ObjectManageOpPostable a)
      )
  => ObjectManageOp a
 where
-- {{{1
  -- 指令调用后的返回报文的类型
  type ObjectManageOpResult a :: *

  -- | 指令编码成一段URL上的路径，记得有 / 开头
  objManageOpUrlPath :: a -> ByteString

  -- | 有些指令需要在 query string 上传入参数．但大部分都不需要
  objManageOpOptions :: a -> Options
  objManageOpOptions _ = defaults

  -- | 不同的指令有时会使用不同的API host，或者有可能使用https
  objManageApiUrl :: a -> String -> String
  objManageApiUrl _ = manageApiUrl

  -- | 接口要post上去的数据类型
  -- 大部分接口只需空的数据，或使用GET，这里使用 ByteString 能适应大部分需求
  type ObjectManageOpPostable a :: *
  type ObjectManageOpPostable a = ByteString

  -- | 有些接口用GET，有些接口用POST
  -- Just 代表 POST，Nothing 代表GET
  objManageApiPostable :: a -> Maybe (ObjectManageOpPostable a)
  default objManageApiPostable :: (Monoid (ObjectManageOpPostable a))
                               => a -> Maybe (ObjectManageOpPostable a)
  objManageApiPostable _ = Just mempty
-- }}}1


data SomeObjectManageOp = forall o. ObjectManageOp o => SomeObjectManageOp o


-- | chstatus 接口用的状态
data ObjectStatus = ObjectEnabled
                  | ObjectDisabled
                  deriving (Show, Eq, Ord, Enum, Bounded)

data ObjChangeStatus = ObjChangeStatus Entry ObjectStatus

instance ObjectManageOp ObjChangeStatus where
  type ObjectManageOpResult ObjChangeStatus = ()

  objManageOpUrlPath (ObjChangeStatus entry status) =
    "/chstatus/" <> encodedEntryUri entry <> "/status/" <> fromString (show (fromEnum status))


data ObjDelete = ObjDelete Entry

instance ObjectManageOp ObjDelete where
  type ObjectManageOpResult ObjDelete = ()

  objManageOpUrlPath (ObjDelete entry) = "/delete/" <> encodedEntryUri entry



data EntryStat =
       EntryStat
         { eStatFsize    :: Int64
         , eStatHash     :: EtagHash
         , eStatMimeType :: Text
         , eStatPutTime  :: ServerTimeStamp
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 5 } ''EntryStat)

data ObjStat = ObjStat Entry

instance ObjectManageOp ObjStat where
  type ObjectManageOpResult ObjStat = EntryStat

  objManageOpUrlPath (ObjStat entry) = "/stat/" <> encodedEntryUri entry

  objManageApiPostable _ = Nothing


data ObjDeleteAfterDays = ObjDeleteAfterDays Entry (Maybe Int)

instance ObjectManageOp ObjDeleteAfterDays where
  type ObjectManageOpResult ObjDeleteAfterDays = ()

  objManageOpUrlPath (ObjDeleteAfterDays entry m_days) =
    "/deleteAfterDays/" <> encodedEntryUri entry <> "/" <> fromString (show (fromMaybe 0 m_days))



data ObjChangeStoreType = ObjChangeStoreType Entry FileStoreType

instance ObjectManageOp ObjChangeStoreType where
  type ObjectManageOpResult ObjChangeStoreType = ()

  objManageOpUrlPath (ObjChangeStoreType entry st) =
    "/chtype/" <> encodedEntryUri entry <> "/type/" <> fromString (show (fromEnum st))


data ObjCopy = ObjCopy Entry Entry

instance ObjectManageOp ObjCopy where
  type ObjectManageOpResult ObjCopy = ()

  objManageOpUrlPath (ObjCopy entry_from entry_to) =
    "/copy/" <> encodedEntryUri entry_from <> "/" <> encodedEntryUri entry_to


data ObjMove = ObjMove Entry Entry

instance ObjectManageOp ObjMove where
  type ObjectManageOpResult ObjMove = ()

  objManageOpUrlPath (ObjMove entry_from entry_to) =
    "/move/" <> encodedEntryUri entry_from <> "/" <> encodedEntryUri entry_to


data ObjChangeMetaCond = MatchFileSize Int64
                       | MatchHash EtagHash
                       | MatchMime MimeType
                       | MatchPutTime ServerTimeStamp

data ObjChangeMeta = ObjChangeMeta
                      Entry
                      (Maybe MimeType)
                      (Map Text Text)
                      [ObjChangeMetaCond]

instance ObjectManageOp ObjChangeMeta where
  type ObjectManageOpResult ObjChangeMeta = ()

  objManageOpUrlPath (ObjChangeMeta entry m_mime custom_kvs conds) =
-- {{{1
    "/chgm/" <> encodedEntryUri entry <> others
    where
      cond_to_kv (MatchFileSize fs) = ("fsize", tshow fs)
      cond_to_kv (MatchHash h)      = ("hash", unEtagHash h)
      cond_to_kv (MatchMime m)      = ("mime", decodeUtf8 m)
      cond_to_kv (MatchPutTime t)   = ("putTime", tshow (intServerTimeStamp t))

      others = mconcat $ catMaybes
                [ flip fmap m_mime $ \ mime -> "/mime/" <> B64U.encode mime
                , if null custom_kvs
                     then Nothing
                     else Just $ mconcat $
                       flip map (mapToList custom_kvs) $ \ (k, v) ->
                         "/x-qn-meta-" <> encodeUtf8 k <> "/" <> B64U.encode (encodeUtf8 v)
                , if null conds
                     then Nothing
                     else Just $ "/cond/"
                          <> B64U.encode
                              (toStrict $ BB.toLazyByteString $
                                renderQueryText False $
                                  map (second Just . cond_to_kv) conds)
                ]
-- }}}1


data ListItem =
       ListItem
         { liKey      :: Text
         , liPutTime  :: ServerTimeStamp
         , liHash     :: EtagHash
         , liFsize    :: Int64
         , liMimeType :: Text
         , liCustomer :: Maybe Text
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListItem)

data ListResult =
       ListResult
         { lrMarker         :: Maybe Text
         , lrCommonPrefixes :: Maybe [Text]
         , lrItems          :: [ListItem]
         }
  deriving (Show)

$(AT.deriveJSON AT.defaultOptions { AT.fieldLabelModifier = lowerFirst . drop 2 } ''ListResult)

data ObjList = ObjList
                Bucket
                (Maybe Int)          -- ^ limit
                (Maybe Text)       -- ^ delimiter
                (Maybe Text)       -- ^ prefix
                (Maybe Text)       -- ^ marker

instance ObjectManageOp ObjList where
-- {{{1
  type ObjectManageOpResult ObjList = ListResult

  objManageOpUrlPath _ = "/list"
  objManageApiUrl _ = manageApiUrlF

  objManageOpOptions (ObjList bucket m_limit m_delimiter m_prefix m_marker) =
    defaults
      & param "bucket" .~ [unBucket bucket]
      & fromMaybe id (flip fmap m_limit $ \ limit -> param "limit" .~ [ tshow $ min 1000 $ max 1 limit ])
      & fromMaybe id (flip fmap m_prefix $ \ prefix -> param "prefix" .~ [prefix])
      & fromMaybe id (flip fmap m_delimiter $ \ delimiter -> param "delimiter" .~ [delimiter])
      & fromMaybe id (flip fmap m_marker $ \ marker -> param "marker" .~ [marker])

  objManageApiPostable _ = Nothing
-- }}}1


-- | See docs: http://developer.qiniu.com/docs/v6/api/reference/rs/fetch.html
-- It seems deprecated.
-- 此为同步版本．异步版本未实现
data ObjFetch = ObjFetch
                  Text    -- ^ from url
                  Scope   -- ^ to

instance ObjectManageOp ObjFetch where
-- {{{1
  type ObjectManageOpResult ObjFetch = UploadedFileInfo

  objManageOpUrlPath (ObjFetch url_from scope_to) =
    "/fetch/" <> B64U.encode (encodeUtf8 url_from)
               <> "/to/" <> encodedScopeUri scope_to

  objManageApiUrl _ url_path = "http://iovip.qbox.me" <> url_path
-- }}}1


data BatchOp = BatchOp [SomeObjectManageOp]

instance ObjectManageOp BatchOp where
-- {{{1
  type ObjectManageOpResult BatchOp = [WsRespBody]

  objManageOpUrlPath _ = "/batch"

  objManageApiPostable (BatchOp some_ops) =
    Just $ intercalate "&" $ map op_path some_ops
    where
      op_path (SomeObjectManageOp op) = "op=" <> objManageOpUrlPath op
-- }}}1


data BucketList = BucketList

instance ObjectManageOp BucketList where
-- {{{1
  type ObjectManageOpResult BucketList = [Text]

  objManageOpUrlPath _ = "/buckets"

  objManageApiPostable _ = Nothing
-- }}}1


data BucketDomainList = BucketDomainList Bucket

instance ObjectManageOp BucketDomainList where
-- {{{1
  type ObjectManageOpResult BucketDomainList = [Text]

  objManageOpUrlPath _ = "/v6/domain/list"

  objManageOpOptions (BucketDomainList bucket) =
    defaults & param "tbl" .~ [ unBucket bucket ]

  objManageApiPostable _ = Nothing

  objManageApiUrl _ = manageApiUrlApi
-- }}}1


manageOpRemoteCall :: (ObjectManageOp o)
                   => o
                   -> QiniuManageMonad m (WsResult (ObjectManageOpResult o))
-- {{{1
manageOpRemoteCall op = runExceptT $ do
  sess <- lift $ lift ask
  (secret_key, access_key) <- ask
  opts <- liftIO $ applyAccessTokenPost secret_key access_key url_path m_post_data defaults

  resp <- ExceptT $ liftIO $ try $
    case m_post_data of
      Nothing -> WS.getWith opts sess url
      Just post_data -> WS.postWith opts sess url post_data

  asWsResponseResult resp `Lifted.onException`
    ($logErrorS logSource $ "Cannot parse response body: " <> toStrict (decodeUtf8 (view responseBody resp)))
  where
    url_path = objManageOpUrlPath op
    url = objManageApiUrl op $ C8.unpack url_path
    m_post_data = objManageApiPostable op
-- }}}1


chstatus :: Entry -> ObjectStatus -> QiniuManageMonad m (WsResult ())
chstatus entry status =
  manageOpRemoteCall (ObjChangeStatus entry status)

delete :: Entry -> QiniuManageMonad m (WsResult ())
delete entry = manageOpRemoteCall (ObjDelete entry)

deleteMaybe :: Entry -> QiniuManageMonad m (WsResult (Maybe ()))
deleteMaybe = fmap maybeDoesNotExist . delete

stat :: Entry -> QiniuManageMonad m (WsResult EntryStat)
stat entry = manageOpRemoteCall (ObjStat entry)

statMaybe :: Entry -> QiniuManageMonad m (WsResult (Maybe EntryStat))
statMaybe = fmap maybeDoesNotExist . stat

changeStoreType :: Entry -> FileStoreType -> QiniuManageMonad m (WsResult ())
changeStoreType entry st = manageOpRemoteCall (ObjChangeStoreType entry st)

changeStoreTypeMaybe :: Entry -> FileStoreType -> QiniuManageMonad m (WsResult (Maybe ()))
changeStoreTypeMaybe = (fmap maybeDoesNotExist .) . changeStoreType

deleteAfterDays :: Entry
                -> Maybe Int  -- ^ Nothing: disable lifecyle
                -> QiniuManageMonad m (WsResult ())
deleteAfterDays entry m_days =
  manageOpRemoteCall (ObjDeleteAfterDays entry m_days)

deleteAfterDaysMaybe :: Entry
                     -> Maybe Int  -- ^ Nothing: disable lifecyle
                     -> QiniuManageMonad m (WsResult (Maybe ()))
deleteAfterDaysMaybe = (fmap maybeDoesNotExist .) . deleteAfterDays


copy :: Entry        -- ^ from
     -> Entry        -- ^ to
     -> QiniuManageMonad m (WsResult ())
copy entry_from entry_to = manageOpRemoteCall (ObjCopy entry_from entry_to)

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
move entry_from entry_to = manageOpRemoteCall (ObjMove entry_from entry_to)

moveMaybe :: Entry        -- ^ from
          -> Entry        -- ^ to
          -> QiniuManageMonad m (WsResult (Maybe (Maybe ())))
          -- ^ Nothing: source does not exist
          --   Just Nothing: dest does not exist
          --   Just (Just ()): success
moveMaybe = (fmap (maybeDoesNotExist . maybeAlreadyExists) .) . move


chgm :: Entry
     -> Maybe MimeType
     -> Map Text Text
     -> [ObjChangeMetaCond]
     -> QiniuManageMonad m (WsResult ())
chgm entry m_mime vars conds =
  manageOpRemoteCall (ObjChangeMeta entry m_mime vars conds)

chgmMaybe :: Entry
          -> Maybe MimeType
          -> Map Text Text
          -> [ObjChangeMetaCond]
          -> QiniuManageMonad m (WsResult (Maybe ()))
chgmMaybe entry m_mime vars conds =
  fmap maybeDoesNotExist $ chgm entry m_mime vars conds


-- | Test whether an entry already exists and with the same etag
alreadyExistsAndMatch :: Entry
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
alreadyExistsAndMatch' :: Entry
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


list :: Bucket
     -> Maybe Int          -- ^ limit
     -> Maybe Text       -- ^ delimiter
     -> Maybe Text       -- ^ prefix
     -> Maybe Text       -- ^ marker
     -> QiniuManageMonad m (WsResult ListResult)
-- {{{1
list bucket limit delimiter prefix marker = do
  manageOpRemoteCall $ ObjList bucket limit delimiter prefix marker
-- }}}1


listSource :: QiniuRemoteCallMonad m
           => Bucket
           -> Maybe Int          -- ^ limit
           -> Maybe Text       -- ^ delimiter
           -> Maybe Text       -- ^ prefix
           -> Source (ReaderT (SecretKey, AccessKey) m) ListResult
            -- ^ may throw HttpException or WsError
-- {{{1
listSource bucket limit delimiter prefix = do
  go Nothing

  where
    nothing_or_null = fromMaybe True . fmap null

    go marker = do
      lr <- lift (list bucket limit delimiter prefix marker)
            >>= either (either throwM throwM) return . packError
      yield lr
      let new_marker = lrMarker lr
      if nothing_or_null new_marker
        then return ()
        else go new_marker
-- }}}1


fetch :: Text         -- ^ from url
      -> Scope        -- ^ to
      -> QiniuManageMonad m (WsResult UploadedFileInfo)
fetch from_url to_scope = manageOpRemoteCall $ ObjFetch from_url to_scope


batch :: [SomeObjectManageOp]
      -> QiniuManageMonad m (WsResult [WsRespBody])
batch some_ops = manageOpRemoteCall $ BatchOp some_ops


buckets :: QiniuManageMonad m (WsResult [Text])
buckets = manageOpRemoteCall BucketList


bucketDomains :: Bucket -> QiniuManageMonad m (WsResult [Text])
bucketDomains = manageOpRemoteCall . BucketDomainList


-- vim: set foldmethod=marker:
