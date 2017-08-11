module Qiniu.Error where

-- {{{1 imports
import ClassyPrelude

import Network.HTTP.Client                  (HttpException(..))
import Qiniu.WS.Types
-- }}}1


isResourceDoesNotExistError :: Either HttpException WsError -> Bool
isResourceDoesNotExistError = testWsErrorCode (== 612)

isResourceAlreadyExistsError :: Either HttpException WsError -> Bool
isResourceAlreadyExistsError = testWsErrorCode (== 614)



-- vim: set foldmethod=marker:
