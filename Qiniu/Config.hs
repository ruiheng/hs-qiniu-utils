{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Config where

-- {{{1 imports
import ClassyPrelude
import Data.Aeson

import Qiniu.Region
import Qiniu.Types
import Qiniu.Class
-- }}}1


check_non_empty_str :: (MonoFoldable a, MonadPlus m) => a -> m a
check_non_empty_str s = if null s then mzero else return s

data QiniuAccountConfig = QiniuAccountConfig
  { qnAccountSecretKey :: SecretKey
  , qnAccountAccessKey :: AccessKey
  }

instance FromJSON QiniuAccountConfig where
  parseJSON = withObject "QiniuAccountConfig" $ \o ->
    QiniuAccountConfig
      <$> (fmap SecretKey $ o .: "secret-key" >>= check_non_empty_str)
      <*> (fmap AccessKey $ o .: "access-key" >>= check_non_empty_str)


data QiniuBucketConfig = QiniuBucketConfig
  { qnBucketName       :: Bucket
  , qnBucketDomain     :: Maybe Text
  , qnBucketSslUrl     :: Bool
  , qnBucketRegion     :: Region
  , qnBucketPathPrefix :: Text
  -- ^ 把 resource key 当路径用时，总是加上这段路径前缀
  }
  deriving (Eq, Show)

-- {{{1 instances
instance FromJSON QiniuBucketConfig where
    parseJSON = withObject "QiniuBucketConfig" $ \o ->
        QiniuBucketConfig
            <$> (fmap Bucket $ o .: "name" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "domain" )
            <*> ( o .:? "ssl-url" .!= False)
            <*> o .: "region"
            <*> ( fmap (fromMaybe "") $ o .:? "path-prefix" )
        where
            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x

instance HasBucket QiniuBucketConfig where getBucket = qnBucketName
-- }}}1


-- | 包含一个公开 bucket, 一个私有 bucket 的设置
data QiniuDualBucketConfig = QiniuDualBucketConfig
  { qcDualPublicBucket  :: Bucket
  , qcDualPublicDomain  :: Maybe Text
  , qcDualPublicSslUrl  :: Bool
  , qcDualPublicRegion  :: Region
  , qcDualPublicPathPrefix :: Text
  , qcDualPrivateBucket :: Bucket
  , qcDualPrivateDomain :: Maybe Text
  , qcDualPrivateSslUrl :: Bool
  , qcDualPrivateRegion :: Region
  , qcDualPrivatePathPrefix :: Text
  }
  deriving (Eq, Show)

-- {{{1 instances
instance FromJSON QiniuDualBucketConfig where
    parseJSON = withObject "QiniuDualBucketConfig" $ \o ->
        QiniuDualBucketConfig
            <$> (fmap (Bucket . fromString) $
                                        o .: "public-bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "public-domain" )
            <*> ( o .:? "public-ssl-url" .!= False )
            <*> (o .: "public-region" <|> o .: "region")
            <*> ( fmap (fromMaybe "") $ liftM2 (<|>) (o .:? "public-path-prefix") (o .:? "path-prefix") )
            <*> (fmap (Bucket . fromString) $
                                        o .: "private-bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "private-domain" )
            <*> ( o .:? "private-ssl-url" .!= False )
            <*> (o .: "private-region" <|> o .: "region")
            <*> ( fmap (fromMaybe "") $ liftM2 (<|>) (o .:? "private-path-prefix") (o .:? "path-prefix") )
        where
            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x
-- }}}1


pubOfQiniuDualBucketConfig :: QiniuDualBucketConfig -> QiniuBucketConfig
pubOfQiniuDualBucketConfig qc = QiniuBucketConfig
                                  (qcDualPublicBucket qc)
                                  (qcDualPublicDomain qc)
                                  (qcDualPublicSslUrl qc)
                                  (qcDualPublicRegion qc)
                                  (qcDualPublicPathPrefix qc)


priOfQiniuDualBucketConfig :: QiniuDualBucketConfig -> QiniuBucketConfig
priOfQiniuDualBucketConfig qc = QiniuBucketConfig
                                  (qcDualPrivateBucket qc)
                                  (qcDualPrivateDomain qc)
                                  (qcDualPrivateSslUrl qc)
                                  (qcDualPrivateRegion qc)
                                  (qcDualPrivatePathPrefix qc)



-- vim: set foldmethod=marker:
