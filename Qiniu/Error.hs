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


maybeDoesNotExists :: WsResult a -> WsResult (Maybe a)
maybeDoesNotExists old_res =
  case packError old_res of
    Left err | isResourceDoesNotExistError err -> Right (Right Nothing)
    _ -> fmap (fmap Just) old_res


-- vim: set foldmethod=marker:
