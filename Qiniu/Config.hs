{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Config where

-- {{{1 imports
import ClassyPrelude
import Data.Aeson

import Qiniu.Types
-- }}}1


data QiniuConfig =
       QiniuConfig
         { qiniuConfigSecretKey  :: SecretKey
         , qiniuConfigAccessKey  :: AccessKey
         , qiniuConfigBucket     :: Bucket
         , qiniuConfigDomain     :: Maybe String
         , qiniuConfigSslUrl     :: Bool
         , qiniuConfigPathPrefix :: String
         -- ^ 把 resource key 当路径用时，总是加上这段路径前缀
         }
  deriving (Eq, Show)

-- {{{1 instances
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
            <*> ( o .:? "ssl-url" .!= False)
            <*> ( fmap (fromMaybe "") $ o .:? "path-prefix" )
        where
            check_non_empty_str s = if null s then mzero else return s

            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x
-- }}}1


-- | 包含一个公开 bucket, 一个私有 bucket 的设置
data QiniuDualConfig =
       QiniuDualConfig
         { qcDualSecretKey     :: SecretKey
         , qcDualAccessKey     :: AccessKey
         , qcDualPublicBucket  :: Bucket
         , qcDualPublicDomain  :: Maybe String
         , qcDualPublicSslUrl  :: Bool
         , qcDualPrivateBucket :: Bucket
         , qcDualPrivateDomain :: Maybe String
         , qcDualPrivateSslUrl :: Bool
         , qcDualPathPrefix    :: String
         }
  deriving (Eq, Show)

-- {{{1 instances
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
            <*> ( o .:? "public-ssl-url" .!= False )
            <*> (fmap (Bucket . fromString) $
                                        o .: "private-bucket" >>= check_non_empty_str)
            <*> ( fmap nullToMaybe $ o .:? "private-domain" )
            <*> ( o .:? "private-ssl-url" .!= False )
            <*> ( fmap (fromMaybe "") $ o .:? "path-prefix" )
        where
            check_non_empty_str s = if null s then mzero else return s

            nullToMaybe Nothing = Nothing
            nullToMaybe (Just x) = if null x then Nothing else Just x
-- }}}1


pubOfQiniuDualConfig :: QiniuDualConfig -> QiniuConfig
-- {{{1
pubOfQiniuDualConfig qc = QiniuConfig
                            (qcDualSecretKey qc)
                            (qcDualAccessKey qc)
                            (qcDualPublicBucket qc)
                            (qcDualPublicDomain qc)
                            (qcDualPublicSslUrl qc)
                            (qcDualPathPrefix qc)
-- }}}1


priOfQiniuDualConfig :: QiniuDualConfig -> QiniuConfig
-- {{{1
priOfQiniuDualConfig qc = QiniuConfig
                            (qcDualSecretKey qc)
                            (qcDualAccessKey qc)
                            (qcDualPrivateBucket qc)
                            (qcDualPrivateDomain qc)
                            (qcDualPrivateSslUrl qc)
                            (qcDualPathPrefix qc)
-- }}}1



-- vim: set foldmethod=marker:
