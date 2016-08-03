module Main where

import ClassyPrelude
import qualified Data.Text as T
import Qiniu.ByteString
import System.Exit                          (exitFailure)

import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Char8      as B8

printETag :: Bool -> FilePath -> IO ()
printETag print_name name = do
    etag <- fmap hetagL $ LB.readFile name
    when print_name $
      putStr $ fromString $ name <> ": "
    B8.putStrLn etag

main :: IO ()
main = do
    fnames <- getArgs
    if null fnames
        then do
            hPutStrLn stderr $ "Usage: qetag <filename> [<filename> ...]"
            exitFailure
        else do
            forM_ (map T.unpack fnames) $ printETag $
                            case fnames of
                                (_:[])  -> False
                                _       -> True
