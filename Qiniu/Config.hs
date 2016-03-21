{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Config where

import Prelude
import Control.Monad
import Data.Aeson
import Data.String

import Qiniu.Types


data QiniuConfig = QiniuConfig {
                    qiniuConfigSecretKey    :: SecretKey
                    , qiniuConfigAccessKey  :: AccessKey
                    , qiniuConfigBucket     :: Bucket
                    , qiniuConfigDomain     :: Maybe String
                    }
                    deriving (Eq, Show)

instance FromJSON QiniuConfig where
    parseJSON = withObject "QiniuConfig" $ \o ->
        QiniuConfig
            <$> (fmap (SecretKey . fromString) $
                                        o .: "secret-key" >>= check_non_empty_str)
            <*> (fmap (AccessKey . fromString) $
                                        o .: "access-key" >>= check_non_empty_str)
            <*> (fmap (Bucket . fromString) $
                                        o .: "bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "domain" )
        where
            check_non_empty_str s = if null s then mzero else return s

            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x


-- | 包含一个公开 bucket, 一个私有 bucket 的设置
data QiniuDualConfig = QiniuDualConfig {
                    qcDualSecretKey         :: SecretKey
                    , qcDualAccessKey       :: AccessKey
                    , qcDualPublicBucket    :: Bucket
                    , qcDualPublicDomain    :: Maybe String
                    , qcDualPrivateBucket   :: Bucket
                    , qcDualPrivateDomain   :: Maybe String
                    }
                    deriving (Eq, Show)

instance FromJSON QiniuDualConfig where
    parseJSON = withObject "QiniuDualConfig" $ \o ->
        QiniuDualConfig
            <$> (fmap (SecretKey . fromString) $
                                        o .: "secret-key" >>= check_non_empty_str)
            <*> (fmap (AccessKey . fromString) $
                                        o .: "access-key" >>= check_non_empty_str)
            <*> (fmap (Bucket . fromString) $
                                        o .: "public-bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "public-domain" )
            <*> (fmap (Bucket . fromString) $
                                        o .: "private-bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "private-domain" )
        where
            check_non_empty_str s = if null s then mzero else return s

            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x


pubOfQiniuDualConfig :: QiniuDualConfig -> QiniuConfig
pubOfQiniuDualConfig qc = QiniuConfig
                            (qcDualSecretKey qc)
                            (qcDualAccessKey qc)
                            (qcDualPublicBucket qc)
                            (qcDualPublicDomain qc)

priOfQiniuDualConfig :: QiniuDualConfig -> QiniuConfig
priOfQiniuDualConfig qc = QiniuConfig
                            (qcDualSecretKey qc)
                            (qcDualAccessKey qc)
                            (qcDualPrivateBucket qc)
                            (qcDualPrivateDomain qc)
