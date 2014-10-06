{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString            as B
import qualified Data.Map                   as Map
import qualified Data.Yaml                  as Y
import qualified Data.ByteString.Char8      as C8
import qualified Control.Monad.Trans.State  as S
import Data.ByteString                      (ByteString)
import Data.String                          (fromString)
import Data.List                            (nub)
import Control.Monad.Trans.Reader           (ReaderT(..), runReaderT)
import Control.Monad.Reader.Class           (ask)
import Control.Monad.Logger                 (MonadLogger, runLoggingT, Loc
                                            , LogLevel(..), defaultLogStr
                                            , LogSource, logInfo)
import System.Log.FastLogger                (pushLogStr, newStderrLoggerSet
                                            , LoggerSet, LogStr)
import Control.Monad.Catch                  (MonadThrow, catch, throwM)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad                        (when)
import Control.Monad.Trans.Control          (MonadBaseControl, control)
import Data.Maybe                           (listToMaybe, catMaybes, fromMaybe)
import Data.Int                             (Int64)
import Numeric                              (readDec)
import Control.Concurrent.Chan              (newChan, writeChan, Chan)
import Control.Concurrent.Async             (withAsync, wait)
import Options.Applicative
import System.IO
import System.Exit
import System.Directory                     (removeFile)
import System.IO.Error                      (isDoesNotExistError)
import Text.Printf                          (printf)

import Qiniu.Types
import Qiniu.WS.Types
import Qiniu.Security
import Qiniu.ByteString
import Qiniu.Upload

data RsyncOptions = RsyncOptions {
                roSecretKey         :: SecretKey
                , roAccessKey       :: AccessKey
                , roBucket          :: Bucket
                , roResourceSaveKey :: Maybe ResourceKey
                , roBlockSize       :: Maybe Int64
                , roChunkSize       :: Maybe Int64
                , roVerbose         :: Int
                , roThreadNum       :: Int
                , roContinueMode    :: Bool
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
                <*> (option auto
                        $ long "thread" <> short 't' <> value 1
                        <> metavar "NUM"
                        <> help "Thread number (default 1)")
                <*> (switch $ long "coninue" <> short 'c'
                            <> help "Coninue/Recover mode.")

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
    pp0 <- mkPutPolicy (Scope bucket Nothing) save_key (fromIntegral (3600*24 :: Int))
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


lookupStateFile :: (MonadIO m) =>
    ByteString  -- ^ etag
    -> m (FilePath, Maybe RecoverUploadInfo)
lookupStateFile etag = liftIO $ do
    let state_fp = ".up_state." ++ C8.unpack etag ++ ".yml"
    err_or_rui <- Y.decodeFileEither state_fp
    case err_or_rui of
        Left _ -> return (state_fp, Nothing)
        Right x -> return (state_fp, Just x)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwM e

uploadOneFileByBlock :: forall m.
    (MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m) =>
    Int64 -> Int64 -> FilePath -> ReaderT RsyncOptions m ()
uploadOneFileByBlock block_size chunk_size fp = do
    ro <- ask
    let skey    = roSecretKey ro
        akey    = roAccessKey ro
        bucket  = roBucket ro
        save_key = roResourceSaveKey ro
        thread_num = roThreadNum ro
        cr_mode     = roContinueMode ro
    pp0 <- mkPutPolicy (Scope bucket Nothing) save_key (fromIntegral (3600*24 :: Int))
    let pp = pp0
    let upload_token = uploadToken skey akey pp
    let rkey = Nothing      -- TODO: customize the resource key to save

    bs <- liftIO $ LB.readFile fp
    let etag = hetagL bs
        empty_rui = RecoverUploadInfo [] block_size chunk_size rkey
    (state_fp, m_rui) <- if cr_mode
                            then lookupStateFile etag
                            else return (".useless.yml", Nothing)
    let last_rui = fromMaybe empty_rui m_rui

    done_ch <- liftIO $ (newChan :: IO (Chan (Maybe (Int64, ChunkPutResult))))
    let on_done = onDoneWriteChan done_ch

    let watch_ch state_file = onDoneChanWatcher done_ch $ \offset cpr -> do
            cpr_map <- S.get
            let new_cpr_map = Map.insert
                                (offset `div` block_size) cpr
                                cpr_map
            let rui = cprMapToRecoverUploadInfo
                        block_size chunk_size save_key new_cpr_map
            when cr_mode $ do
                liftIO $ B.writeFile state_file $ Y.encode rui
            let done_len = doneBytesLength $ map snd $ Map.toList new_cpr_map
                total_len = LB.length bs
            $(logInfo) $ fromString $
                show done_len
                <> " bytes of total "
                <> show total_len
                <> " has been uploaded. ("
                <> printf "%.02f" (fromIntegral (done_len * 100) / fromIntegral total_len :: Double)
                <> "%)"
            S.put new_cpr_map

    let init_map = cprMapFromRecoverUploadInfo last_rui

    ws_result <- control $ \run_in_base -> do
                    withAsync
                        (run_in_base $ S.runStateT (watch_ch state_fp) init_map)
                        $ \watcher -> do
                            ws_result <- run_in_base $ runReaderT
                                                        (uploadByBlocksContinue
                                                            (nopOnWsCallError 3)
                                                            on_done
                                                            thread_num
                                                            last_rui
                                                            bs)
                                                        upload_token

                            writeChan done_ch Nothing >> wait watcher >> return ()
                            return ws_result

    liftIO $ do
        case unpackError ws_result of
            Left http_err -> do
                hPutStrLn stderr $ "HTTP Error: " ++ show http_err
            Right (Left err) -> do
                hPutStrLn stderr $ "Web Service Error: " ++ show err
            Right (Right (UploadedFileInfo r_etag rrkey)) -> do
                let scope = Scope bucket (Just rrkey)
                putStrLn $ "File '" ++ fp ++ "' uploaded to " ++ show scope
                putStrLn $ "ETag: " ++ r_etag

                -- delete state file only server reports success
                when cr_mode $ do
                    removeIfExists state_fp

                when (r_etag /= C8.unpack etag) $ do
                    hPutStrLn stderr $ "etag mismatch"
                    exitFailure

start :: (MonadIO m, MonadThrow m, MonadLogger m, MonadBaseControl IO m) =>
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
