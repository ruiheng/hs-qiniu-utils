{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Region 
  ( Region(..)
  , Action(..)
  , regionNickname
  , regionHostSuffix
  , getBaseUrl
  ) where

-- {{{1 imports
import           ClassyPrelude
import           Data.Aeson.TH
-- }}}1


-- | see: https://developer.qiniu.com/kodo/manual/1671/region-endpoint
data Region = EastChina | NorthChina | SouthChina | NorthAmerica | Singapore
              deriving (Eq, Ord, Enum, Bounded, Show, Read)

$(deriveJSON defaultOptions ''Region)


data Action = ServerUpload | ClientUpload | Download
              deriving (Eq, Ord, Enum, Bounded, Show, Read)


regionNickname :: IsString a => Region -> a
regionNickname EastChina    = "z0"
regionNickname NorthChina   = "z1"
regionNickname SouthChina   = "z2"
regionNickname NorthAmerica = "na0"
regionNickname Singapore    = "as0"


regionHostSuffix :: IsString a => Region -> Maybe a
regionHostSuffix EastChina = Nothing
regionHostSuffix r         = Just $ regionNickname r


basehost :: (Semigroup a, IsString a) => Action -> Bool -> (a, a)
-- {{{1
basehost ServerUpload = bool
              ("http://up", ".qiniu.com")
              ("https://up", ".qbox.me")

basehost ClientUpload = bool
              ("http://upload", ".qiniu.com")
              ("https://upload", ".qbox.me")

basehost Download = flip addSchema ("://iovip", ".qbox.me")
  where schema = bool "http" "https"
        addSchema = first . (<>) . schema
-- }}}1


getBaseUrl :: (IsString a, Semigroup a)
           => Region
           -> Action
           -> Bool   -- ^ use https or not
           -> a
getBaseUrl region = (addRegion .) . basehost
  where addRegion (prefix, suffix) = case regionHostSuffix region of
                                       Just nn -> prefix <> "-" <> nn <> suffix
                                       Nothing -> prefix <> suffix


-- vim: set foldmethod=marker:
