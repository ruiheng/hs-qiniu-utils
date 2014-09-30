{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude
import qualified Data.ByteString.Lazy       as LB
-- import qualified Data.ByteString.Char8      as C8
import Data.String                          (fromString)
import Data.List                            (nub)
import Control.Monad.Trans.Reader           (ReaderT(..), runReaderT, ask)
import Control.Monad.Logger                 (MonadLogger, runLoggingT, Loc
                                            , LogLevel(..), defaultLogStr
                                            , LogSource, logInfo)
import System.Log.FastLogger                (pushLogStr, newStderrLoggerSet
                                            , LoggerSet, LogStr)
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Data.Maybe                           (listToMaybe, catMaybes)
import Data.Int                             (Int64)
import Numeric                              (readDec)
import Options.Applicative
import System.IO

import Qiniu.Types
import Qiniu.WS.Types
import Qiniu.Security
import Qiniu.Upload

data RsyncOptions = RsyncOptions {
                roSecretKey         :: SecretKey
                , roAccessKey       :: AccessKey
                , roBucket          :: Bucket
                , roResourceSaveKey :: Maybe ResourceKey
                , roBlockSize       :: Maybe Int64
                , roChunkSize       :: Maybe Int64
                , roVerbose         :: Int
                }
                deriving (Show)

appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
appLogger logger_set verbose loc src level ls = do
    let should_log = case level of
                        LevelOther {}   -> True
                        _               -> level `elem` lv_by_v verbose

    if should_log
        then pushLogStr logger_set $ defaultLogStr loc src level ls
        else return ()
    where
        lv_by_v lv
            | lv <= 0   = [ LevelError]
            | lv == 1   = [ LevelError, LevelWarn ]
            | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
            | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]

byteSizeReader :: (Monad m, Num a, Eq a) => String -> m a
byteSizeReader s = do
    maybe (fail "not a valid byte length") (return . (\(x,y) -> x * fromIntegral y)) $
        listToMaybe $
            catMaybes $
            map (\(x, y) -> fmap (x,) $ conv_unit y) $
            readDec s
    where
        conv_unit x
            |x == "" || x == ""     = Just $ (1 :: Int)
            |x == "b" || x == "B"   = Just $ (1 :: Int)
            |x == "k" || x == "K"   = Just $ 1024
            |x == "m" || x == "M"   = Just $ 1024 * 1024
            |x == "g" || x == "G"   = Just $ 1024 * 1024 * 1024
            | otherwise             = Nothing

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
                -- 实测证明：block size 只能是 4M，除非文件只分出一个 block
                <*> (optional $ option byteSizeReader
                        $ long "block"
                        <> metavar "SIZE"
                        <> value (1024 * 1024 * 4)
                        <> help "Block Size. e.g. 1024k 1m. N.B. Must be 4M (which is the default) for now")
                <*> (optional $ option byteSizeReader
                        $ long "chunk"
                        <> metavar "SIZE"
                        <> help "Chunk Size. e.g. 1024k 1m. Specify a chunk size will activate incremental upload mode.")
                <*> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")

parseFileNames :: Parser [FilePath]
parseFileNames = fmap nub $ some $ argument str $ metavar "FILES..."

uploadOneFile :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    FilePath -> ReaderT RsyncOptions m ()
uploadOneFile fp = do
    ro <- ask
    let skey    = roSecretKey ro
        akey    = roAccessKey ro
        bucket  = roBucket ro
        save_key = roResourceSaveKey ro
    pp0 <- mkPutPolicy bucket save_key (fromIntegral (3600*24 :: Int))
    let pp = pp0
    let upload_token = uploadToken skey akey pp
    let rkey = Nothing
    ws_result <- (liftIO $ LB.readFile fp)
                    >>= flip runReaderT upload_token . (uploadOneShort rkey (Just fp))
    liftIO $ do
        case ws_result of
            Left http_err -> do
                hPutStrLn stderr $ "HTTP Error: " ++ show http_err
            Right (Left err) -> do
                hPutStrLn stderr $ "Web Service Error: " ++ show err
            Right (Right (UploadedFileInfo etag rrkey)) -> do
                let scope = Scope bucket (Just rrkey)
                putStrLn $ "File '" ++ fp ++ "' uploaded to " ++ show scope
                putStrLn $ "ETag: " ++ etag

uploadOneFileByBlock :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    Int64 -> Int64 -> FilePath -> ReaderT RsyncOptions m ()
uploadOneFileByBlock block_size chunk_size fp = do
    ro <- ask
    let skey    = roSecretKey ro
        akey    = roAccessKey ro
        bucket  = roBucket ro
        save_key = roResourceSaveKey ro
    pp0 <- mkPutPolicy bucket save_key (fromIntegral (3600*24 :: Int))
    let pp = pp0
    let upload_token = uploadToken skey akey pp
    let rkey = Nothing

    let on_done offset cpr = do
            $(logInfo) $ fromString $
                "block offset " ++ show offset
                    ++ " chunk offset " ++ show (cprOffset cpr)
                    ++ " done."
    ws_result <- (liftIO $ LB.readFile fp)
                >>= flip runReaderT upload_token
                        . (uploadByBlocks
                            nopErrorReporter on_done
                            block_size chunk_size rkey)
    liftIO $ do
        case unpackError ws_result of
            Left http_err -> do
                hPutStrLn stderr $ "HTTP Error: " ++ show http_err
            Right (Left err) -> do
                hPutStrLn stderr $ "Web Service Error: " ++ show err
            Right (Right (UploadedFileInfo etag rrkey)) -> do
                let scope = Scope bucket (Just rrkey)
                putStrLn $ "File '" ++ fp ++ "' uploaded to " ++ show scope
                putStrLn $ "ETag: " ++ etag

start :: (MonadIO m, MonadThrow m, MonadLogger m) =>
    [FilePath] -> ReaderT RsyncOptions m ()
start fps = do
    ro <- ask
    case (roBlockSize ro, roChunkSize ro) of
        (Just block_size, Just chunk_size) ->
                                mapM_ (uploadOneFileByBlock block_size chunk_size) fps
        _ -> mapM_ uploadOneFile fps

start' :: RsyncOptions -> [FilePath] -> IO ()
start' ro fps = do
    logger_set <- newStderrLoggerSet 0
    runLoggingT
        (runReaderT (start fps) ro)
        (appLogger logger_set (roVerbose ro))

main :: IO ()
main = execParser opts >>= uncurry start'
    where
        opts = info (helper <*> ((,) <$> parseOptions <*> parseFileNames))
                (fullDesc
                    <> progDesc "Sync files to QiNiu"
                    <> header "qrsync - remote sync for QiNiu")
