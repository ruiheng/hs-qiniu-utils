module Qiniu.ByteString where

import ClassyPrelude
import Data.Bits                            ((.|.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base64.URL as B64U
import qualified Crypto.Hash.SHA1           as SHA1


hetagChunkBits :: Int
hetagChunkBits = 22

hetagChunkSize :: Int
hetagChunkSize = 2 ^ hetagChunkBits

hetag :: ByteString -> ByteString
hetag bs = B64U.encode $
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

-- | lazy version of hetag
hetagL :: LB.ByteString -> ByteString
hetagL bs = B64U.encode $
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

hetag' :: ByteString -> String
hetag' = C8.unpack . hetag

hetagL' :: LB.ByteString -> String
hetagL' = C8.unpack . hetagL
