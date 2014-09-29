{-# LANGUAGE OverloadedStrings #-}
module Qiniu.Types where

import Prelude
import Data.Time                            (UTCTime)
import Data.Time.Clock.POSIX                (utcTimeToPOSIXSeconds)
import Data.Aeson                           (ToJSON, toJSON, object, (.=))
import Data.Int                             (Int64)
import Data.ByteString                      (ByteString)
import Data.Time                            (getCurrentTime, NominalDiffTime
                                            , addUTCTime)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Data.String                          (IsString)


newtype Bucket = Bucket { unBucket :: String }
                deriving (Eq, Ord, Show)

newtype ResourceKey = ResourceKey { unResourceKey :: String }
                deriving (Eq, Ord, Show)

data Scope = Scope Bucket (Maybe ResourceKey)
                deriving (Eq, Ord)

instance Show Scope where
    show (Scope bucket m_key) =
        case m_key of
            Nothing -> unBucket bucket
            Just key -> concat [ unBucket bucket, ":", unResourceKey key ]

instance ToJSON Scope where
    toJSON = toJSON . show

data PutPolicy = PutPolicy {
                    ppScope             :: Scope
                    , ppDeadline        :: UTCTime
                }

instance ToJSON PutPolicy where
    toJSON pp =
        object
            [ "scope"       .= ppScope pp
            , "deadline"    .= (round $ utcTimeToPOSIXSeconds $ ppDeadline pp :: Int64)
            ]


mkPutPolicy :: MonadIO m => Bucket -> Maybe ResourceKey -> NominalDiffTime -> m PutPolicy
mkPutPolicy bucket m_key dt = liftIO $ do
    now <- getCurrentTime
    let t = addUTCTime dt now
    return $ PutPolicy (Scope bucket m_key) t

newtype SecretKey = SecretKey { unSecretKey :: ByteString }
                    deriving (Eq, Ord, Show)

newtype AccessKey = AccessKey { unAccessKey :: ByteString }
                    deriving (Eq, Ord, Show)


logSource :: IsString a => a
logSource = "QiNiu"
