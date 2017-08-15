{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Qiniu.WebPlayer where

-- {{{1 imports
import           ClassyPrelude
import qualified Data.Aeson.TH as AT

#if defined(YESOD)
import           Yesod.Core
#endif

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



webPlayCssUrl :: IsString s => s
webPlayCssUrl = "https://player.qiniucc.com/sdk/latest/qiniuplayer.min.css"


webPlayJsUrl :: IsString s => s
webPlayJsUrl = "https://player.qiniucc.com/sdk/latest/qiniuplayer.min.js"



#if defined(YESOD)

webPlayWidget :: (MonadWidget m) => Text -> WebPlayerOptions -> m ()
-- {{{1
webPlayWidget element_id opts = do
  -- addStylesheetRemote webPlayCssUrl
  addScriptRemote webPlayJsUrl

  toWidget $ 
    [julius|
      new QiniuPlayer(#{toJSON element_id}, #{toJSON opts});
    |]

  toWidget $ 
    [hamlet|
      <video id=#{element_id} .video-js .vjs-big-play-centered>
    |]
-- }}}1

#endif



-- vim: set foldmethod=marker:
