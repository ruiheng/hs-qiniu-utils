{-# LANGUAGE TemplateHaskell #-}
module Qiniu.WebPlayer where

-- {{{1 imports
import           ClassyPrelude
import qualified Data.Aeson.TH as AT

import           Qiniu.Utils
-- }}}1


data WebPlayerEngine = WebPlayerEngineHtml5
                     | WebPlayerEngineFlash
                     deriving (Show, Eq, Ord, Enum, Bounded)

$(AT.deriveJSON
    AT.defaultOptions {AT.constructorTagModifier = toLower . drop 15}
    ''WebPlayerEngine)


data WebPlayerVideoType = WebPlayerVideoHLS
                        | WebPlayerVideoMP4
                        deriving (Show, Eq, Ord, Enum, Bounded)

$(AT.deriveJSON
    AT.defaultOptions {AT.constructorTagModifier = toLower . drop 14}
    ''WebPlayerVideoType)


data WebPlayerPreload = WebPlayerPreloadAuto
                      | WebPlayerPreloadMeta
                      | WebPlayerPreloadNone
                      deriving (Show, Eq, Ord, Enum, Bounded)

$(AT.deriveJSON
    AT.defaultOptions {AT.constructorTagModifier = toLower . drop 16}
    ''WebPlayerPreload)


data WebPlayerStretch = WebPlayerStretchLetterBox
                      | WebPlayerStretchPanScan
                      | WebPlayerStretchFitWindow
                      | WebPlayerStretchNone
                      deriving (Show, Eq, Ord, Enum, Bounded)

$(AT.deriveJSON
    AT.defaultOptions {AT.constructorTagModifier = toLower . drop 16}
    ''WebPlayerStretch)


data WebPlayerOptions =
     -- CAUTION: 注意字段名为了让 TH 生成的 ToJSON 与文档相符，所以注意不能随便改名
       WebPlayerOptions
         { wpOptsEngineOrder :: [WebPlayerEngine]
         , wpOptsAutoplay    :: Bool
         , wpOptsPreload     :: WebPlayerPreload
         , wpOptsControls    :: Bool
         , wpOptsPoster      :: String
         , wpOptsWidth       :: Int
         , wpOptsHeight      :: Int
         , wpOptsUrl         :: String
         , wpOptsType        :: WebPlayerVideoType
         , wpOptsLoop        :: Bool
         , wpOptsStretching  :: WebPlayerStretch
         }
         deriving (Show)

$(AT.deriveJSON
    AT.defaultOptions {AT.fieldLabelModifier = lowerFirst . drop 5}
    ''WebPlayerOptions)

-- vim: set foldmethod=marker:
