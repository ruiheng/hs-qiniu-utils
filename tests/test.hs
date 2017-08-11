{-# LANGUAGE OverloadedStrings #-}
module Main where

-- {{{1
import ClassyPrelude
import System.Exit
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Base16     as B16
import Network.URI                          (unEscapeString)

import Qiniu
-- }}}1

testPutPolicy :: IO ()
testPutPolicy = do
    let json_pp = "{\"scope\":\"my-bucket:sunflower.jpg\",\"deadline\":1451491200,\"returnBody\":\\\"{\"name\\\":$(fname),\\\"size\\\":$(fsize),\\\"w\\\":$(imageInfo.width),\\\"h\\\":$(imageInfo.height),\\\"hash\\\":$(etag)}\"}"
    let encoded_pp = B64U.encode json_pp
    putStrLn $ "encoded_pp=" ++ tshow (C8.unpack encoded_pp)
    let skey = SecretKey "MY_SECRET_KEY"
    let signed_pp = sign skey encoded_pp
    putStrLn $ fromString $ "signed_pp hex=" ++ C8.unpack (B16.encode signed_pp)
    putStrLn $ fromString $ "signed_pp base64=" ++ C8.unpack (B64U.encode signed_pp)
    putStrLn $ fromString $ "signed_test hex=" <> C8.unpack (B16.encode $ sign (SecretKey "") "")
    putStrLn $ fromString $ "signed_test hex=" <> C8.unpack (B16.encode $ sign (SecretKey "key") "The quick brown fox jumps over the lazy dog")

testEscapeKey :: IO ()
testEscapeKey = do
    go "/a//b/c"
    go "/a//b/c/"
    go "//a///b/c//"
    where
        go k = do
            let path = keyToUrlPath (ResourceKey k)
            if unEscapeString path /= ('/' : k)
                then do
                    putStrLn $ "key " <> tshow k
                                <> " escape result is wrong: " <> tshow path
                    putStrLn $ tshow $ unEscapeString path
                    exitFailure
                else return ()

main :: IO ()
main = do
    testPutPolicy
    testEscapeKey
