{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Qiniu.Region 
  ( Region(..)
  , Action(..)
  , getHost
  ) where

import           ClassyPrelude
import           Data.Aeson.TH

data Region = EastChina | NorthChina | SouthChina | NorthAmerica
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

$(deriveJSON defaultOptions ''Region)

data Action = ServerUpload | ClientUpload | Download

nickname :: IsString a => Region -> Maybe a
nickname EastChina = Nothing
nickname NorthChina = Just "z1"
nickname SouthChina = Just "z2"
nickname NorthAmerica = Just "na0"

basehost :: (Semigroup a, IsString a) => Action -> Bool -> (a, a)
basehost ServerUpload = bool
              ("http://up", ".qiniu.com")
              ("https://up", ".qbox.me")

basehost ClientUpload = bool
              ("http://upload", ".qiniu.com")
              ("https://upload", ".qbox.me")

basehost Download = flip addSchema ("://iovip", ".qbox.me")
  where schema = bool "http" "https"
        addSchema = first . (<>) . schema

getHost :: (IsString a, Semigroup a) => Region -> Action -> Bool -> a
-- region -> secure -> host
getHost region = (addRegion .) . basehost
  where addRegion (prefix, suffix) = case nickname region of
                                       Just nn -> prefix <> "-" <> nn <> suffix
                                       Nothing -> prefix <> suffix

