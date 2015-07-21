module Qiniu.Error where

import Prelude

import Qiniu.WS.Types


isResourceDoesNotExistError :: WsError -> Bool
isResourceDoesNotExistError = (== 612) . wsHttpCode

