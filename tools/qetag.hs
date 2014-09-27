module Main where

import Prelude
import Qiniu.ByteString
import System.Environment
import System.Exit                          (exitFailure)
import System.IO                            (stderr, hPutStrLn)
import Control.Monad                        (when, forM_)

import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Char8      as B8

printETag :: Bool -> FilePath -> IO ()
printETag print_name name = do
    etag <- fmap hetagL $ LB.readFile name
    when print_name $
        putStr $ name ++ ": "
    B8.putStrLn etag

main :: IO ()
main = do
    fnames <- getArgs
    if null fnames
        then do
            hPutStrLn stderr $ "Usage: qetag <filename> [<filename> ...]"
            exitFailure
        else do
            forM_ fnames $ printETag $
                            case fnames of
                                (_:[])  -> False
                                _       -> True
