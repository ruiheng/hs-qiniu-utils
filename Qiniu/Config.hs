{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Config where

import Prelude
import Control.Applicative
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
