module Main where

import ClassyPrelude
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit                          (exitFailure)

import qualified Data.ByteString.Lazy       as LB

import Qiniu.Types
import Qiniu.ByteString

printETag :: Bool -> FilePath -> IO ()
printETag print_name name = do
    etag <- fmap hetagL $ LB.readFile name
    when print_name $
      putStr $ fromString $ name <> ": "
    putStrLn $ fromString $ unEtagHash etag

main :: IO ()
main = do
    fnames <- getArgs
    if null fnames
        then do
            T.hPutStrLn stderr $ fromString $ "Usage: qetag <filename> [<filename> ...]"
            exitFailure
        else do
            forM_ (map T.unpack fnames) $ printETag $
                            case fnames of
                                (_:[])  -> False
                                _       -> True
