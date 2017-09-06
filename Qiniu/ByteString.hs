module Qiniu.ByteString where

-- {{{1 imports
import           ClassyPrelude
import qualified Crypto.Hash.SHA1           as SHA1
import           Data.Bits                  ((.|.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB

import Qiniu.Types
-- }}}1


hetagChunkBits :: Int
hetagChunkBits = 22

hetagChunkSize :: Int
hetagChunkSize = 2 ^ hetagChunkBits

hetag :: ByteString -> EtagHash
-- {{{1
hetag bs = EtagHash $ C8.unpack $ B64U.encode $
    if B.length bs <= hetagChunkSize
        then B.cons flag $ SHA1.hash bs
        else B.cons (flag .|. 0x80) $ SHA1.hash $ go bs
    where
        flag = fromIntegral hetagChunkBits
        go x =
            if B.null t
                then hh
                else B.append hh $ go t
            where
                (h, t) = B.splitAt hetagChunkSize x
                hh = SHA1.hash h
-- }}}1

-- | lazy version of hetag
hetagL :: LB.ByteString -> EtagHash
-- {{{1
hetagL bs = EtagHash $ C8.unpack $ B64U.encode $
    if LB.length bs <= fromIntegral hetagChunkSize
        then B.cons flag $ SHA1.hashlazy bs
        else B.cons (flag .|. 0x80) $ SHA1.hash $ go bs
    where
        flag = fromIntegral hetagChunkBits
        go x =
            if LB.null t
                then hh
                else B.append hh $ go t
            where
                (h, t) = LB.splitAt (fromIntegral hetagChunkSize) x
                hh = SHA1.hashlazy h
-- }}}1



-- vim: set foldmethod=marker:
