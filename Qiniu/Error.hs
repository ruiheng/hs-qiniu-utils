module Qiniu.Error where

import Prelude

import Network.HTTP.Client                  (HttpException(..))
import Qiniu.WS.Types


isResourceDoesNotExistError :: Either HttpException WsError -> Bool
isResourceDoesNotExistError = testWsErrorCode (== 612)

isResourceAlreadyExistsError :: Either HttpException WsError -> Bool
isResourceAlreadyExistsError = testWsErrorCode (== 614)
