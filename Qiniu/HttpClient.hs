module Qiniu.HttpClient where

import Prelude
import qualified Blaze.ByteString.Builder   as BB
import qualified Data.ByteString            as B

import Data.Monoid                          (mempty, (<>))
import Data.IORef                           (newIORef, writeIORef, readIORef)
import Network.HTTP.Client                  ( Request, RequestBody(..) , GivesPopper
                                            , requestHeaders
                                            )
import Network.HTTP.Types                   (Header)

-- import qualified Network.HTTP.Client        as HC

-- | extract a bytestring from RequestBody
requestBodyToBsBuilder :: RequestBody -> IO BB.Builder
requestBodyToBsBuilder (RequestBodyLBS bs)      = return $ BB.fromLazyByteString bs
requestBodyToBsBuilder (RequestBodyBS bs)       = return $ BB.fromByteString bs
requestBodyToBsBuilder (RequestBodyBuilder _ b) = return $ b
requestBodyToBsBuilder (RequestBodyStream _ gp) = readAllFromGivesPopper gp
requestBodyToBsBuilder (RequestBodyStreamChunked gp) = readAllFromGivesPopper gp

readAllFromGivesPopper :: GivesPopper () -> IO BB.Builder
readAllFromGivesPopper gp = do
    result <- newIORef mempty
    let go b read_bs = do
            bs <- read_bs 
            if B.null bs
                then writeIORef result b >> return ()
                else go (b <> BB.fromByteString bs) read_bs
    gp $ go mempty
    readIORef result


replaceReqHttpHeader ::
    Header
    -> Request
    -> Request
replaceReqHttpHeader new_header req =
    req { requestHeaders = f (requestHeaders req) }
    where
        f headers = new_header : filter ((/= fst new_header ) . fst) headers 
