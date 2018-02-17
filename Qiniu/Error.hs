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


maybeOfMatchedError :: (Either HttpException WsError -> Bool)
                    -> WsResult a
                    -> WsResult (Maybe a)
maybeOfMatchedError match_err old_res =
  case packError old_res of
    Left err | match_err err -> Right (Right Nothing)
    _ -> fmap (fmap Just) old_res


maybeDoesNotExist :: WsResult a -> WsResult (Maybe a)
maybeDoesNotExist = maybeOfMatchedError isResourceDoesNotExistError


maybeAlreadyExists :: WsResult a -> WsResult (Maybe a)
maybeAlreadyExists = maybeOfMatchedError isResourceAlreadyExistsError


-- vim: set foldmethod=marker:
