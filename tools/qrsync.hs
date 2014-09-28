{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Char8      as C8
import Data.String                          (fromString)
import Data.List                            (nub)
import Control.Monad.Trans.Reader           (ReaderT(..), runReaderT, ask)
import Control.Monad.IO.Class               (liftIO)
import Options.Applicative
import System.IO

import Qiniu.Types
import Qiniu.Security
import Qiniu.Upload

data RsyncOptions = RsyncOptions {
                roSecretKey         :: SecretKey
                , roAccessKey       :: AccessKey
                , roBucket          :: Bucket
                , roResourceSaveKey :: Maybe ResourceKey
                , roVerbose         :: Int
                }
                deriving (Show)

parseOptions :: Parser RsyncOptions
parseOptions = RsyncOptions <$>
                (fmap (SecretKey . fromString) $
                    strOption $ long "skey" <> short 's' <> help "Secret Key")
                <*> (fmap (AccessKey . fromString) $
                        strOption $ long "akey" <> short 'a' <> help "Access Key")
                <*> (fmap Bucket $
                        strOption $ long "bucket" <> short 'b' <> help "Bucket")
                <*> (optional $ fmap ResourceKey $
                        strOption $ long "save-key" <> short 'S'
                            <> help "Resource Save Key")
                <*> (option
#if MIN_VERSION_optparse_applicative(0, 10, 0)
                        auto
#endif
                        $ long "verbose" <> short 'v' <> value 0 <> help "Verbose Level")

parseFileNames :: Parser [FilePath]
parseFileNames = fmap nub $ many $ argument str $ metavar "FILES..."

uploadOneFile :: FilePath -> ReaderT RsyncOptions IO ()
uploadOneFile fn = do
    ro <- ask
    let skey    = roSecretKey ro
        akey    = roAccessKey ro
        bucket  = roBucket ro
        save_key = roResourceSaveKey ro
    pp0 <- mkPutPolicy bucket save_key (fromIntegral (3600*24 :: Int))
    let pp = pp0
    let upload_token = uploadToken skey akey pp
    let rkey = Nothing
    liftIO $ do
        ws_result <- LB.readFile fn >>= uploadOneShort upload_token rkey (Just fn)
        case ws_result of
            Left http_err -> do
                hPutStrLn stderr $ "HTTP Error: " ++ show http_err
            Right (Left err) -> do
                hPutStrLn stderr $ "Web Service Error: " ++ show err
            Right (Right (etag, rrkey)) -> do
                let scope = Scope bucket (Just rrkey)
                putStrLn $ "File '" ++ fn ++ "' uploaded to " ++ show scope
                putStrLn $ "ETag: " ++ C8.unpack etag

start :: [FilePath] -> ReaderT RsyncOptions IO ()
start fps = do
    mapM_ uploadOneFile fps

main :: IO ()
main = execParser opts >>= \(ro, fps) -> runReaderT (start fps) ro
    where
        opts = info (helper <*> ((,) <$> parseOptions <*> parseFileNames))
                (fullDesc
                    <> progDesc "Sync files to QiNiu"
                    <> header "qrsync - remote sync for QiNiu")
