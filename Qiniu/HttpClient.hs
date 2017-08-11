module Qiniu.HttpClient where

-- {{{1 imports
import ClassyPrelude
import qualified Blaze.ByteString.Builder   as BB
import qualified Data.ByteString            as B

import Network.HTTP.Client                  ( Request, RequestBody(..) , GivesPopper
                                            , requestHeaders
                                            )
import Network.HTTP.Types                   (Header)

-- import qualified Network.HTTP.Client        as HC
-- }}}1

-- | extract a bytestring from RequestBody
requestBodyToBsBuilder :: RequestBody -> IO BB.Builder
-- {{{1
requestBodyToBsBuilder (RequestBodyLBS bs)      = return $ BB.fromLazyByteString bs
requestBodyToBsBuilder (RequestBodyBS bs)       = return $ BB.fromByteString bs
requestBodyToBsBuilder (RequestBodyBuilder _ b) = return $ b
requestBodyToBsBuilder (RequestBodyStream _ gp) = readAllFromGivesPopper gp
requestBodyToBsBuilder (RequestBodyStreamChunked gp) = readAllFromGivesPopper gp
#if MIN_VERSION_http_client(0, 4, 12)
requestBodyToBsBuilder (RequestBodyIO get_body) = get_body >>= requestBodyToBsBuilder
#endif
-- }}}1

readAllFromGivesPopper :: GivesPopper () -> IO BB.Builder
-- {{{1
readAllFromGivesPopper gp = do
    result <- newIORef mempty
    let go b read_bs = do
            bs <- read_bs 
            if B.null bs
                then writeIORef result b >> return ()
                else go (b <> BB.fromByteString bs) read_bs
    gp $ go mempty
    readIORef result
-- }}}1


replaceReqHttpHeader :: Header
                     -> Request
                     -> Request
-- {{{1
replaceReqHttpHeader new_header req =
    req { requestHeaders = f (requestHeaders req) }
    where
        f headers = new_header : filter ((/= fst new_header ) . fst) headers 
-- }}}1



-- vim: set foldmethod=marker:
