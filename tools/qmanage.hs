{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude
import qualified Data.ByteString.Char8      as C8

import Options.Applicative
import System.IO
import System.IO.Error                      (isEOFError)

import Data.String                          (fromString)
import Data.ByteString                      (ByteString)
import Control.Monad.Logger                 (MonadLogger, runLoggingT, Loc
                                            , LogLevel(..), defaultLogStr
                                            , LogSource, logInfo)
import System.Log.FastLogger                (pushLogStr, newStderrLoggerSet
                                            , LoggerSet, LogStr)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad.Trans.Class            (lift)
import Control.Monad.Catch                  (MonadCatch, try)
import Control.Monad.Trans.Reader           (ReaderT(..), runReaderT)
import Control.Monad.Reader                 (MonadReader, ask)

import qualified Text.Parsec                as TP
import qualified Text.Parsec.Token          as TPT
import Text.Parsec.Language                 (haskellDef)
import Data.Char                            (isSpace, isAlphaNum)
import Control.Monad                        (void)
import Network.HTTP.Client                  (withManager, Manager
                                            , defaultManagerSettings
                                            )
import Data.Conduit                         (($$))
import qualified Data.Conduit.List          as CL

import Qiniu.Types
import Qiniu.WS.Types
import Qiniu.Manage


-- | 我们会用 Parsec3 里的工具解释用户输入的指令
type CharParser = TP.Parsec String ()

data ManageOptions = ManageOptions {
                moSecretKey         :: SecretKey
                , moAccessKey       :: AccessKey
                , moVerbose         :: Int
                }
                deriving (Show)

parseOptions :: Parser ManageOptions
parseOptions = ManageOptions <$>
                (fmap (SecretKey . fromString) $
                    strOption $ long "skey" <> short 's' <> help "Secret Key")
                <*> (fmap (AccessKey . fromString) $
                        strOption $ long "akey" <> short 'a' <> help "Access Key")
                <*> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")


-- | 这个管理工具支持的所有命令
data Command = Stat Entry
            | Delete Entry
            | Copy Entry Entry
            | Move Entry Entry
            | ChangeMime Entry ByteString
            | List Bucket String
            deriving (Show)

parseCommand :: CharParser (Maybe Command)
parseCommand = do
    TP.spaces
    cmd <- TP.many $ TP.satisfy (not . isSpace)
    _ <- (void $ TP.many1 TP.space) <|> TP.eof
    if null cmd
        then return Nothing
        else case cmd of
            "stat"  -> Just . Stat <$> p_entry <* (TP.spaces >> TP.eof)
            "delete"-> Just . Delete <$> p_entry <* (TP.spaces >> TP.eof)
            "copy"  -> Just . uncurry Copy <$> p_two_entries <* (TP.spaces >> TP.eof)
            "move"  -> Just . uncurry Move <$> p_two_entries <* (TP.spaces >> TP.eof)
            "chgm"  -> Just <$> p_chgm <* (TP.spaces >> TP.eof)
            "list"  -> Just <$> p_list <* (TP.spaces >> TP.eof)
            _       -> fail $ "unknown command: " ++ show cmd
    where
        p_bucket = do
            fmap Bucket $ TP.many1 $ TP.satisfy (\x -> isAlphaNum x || x == '-')

        p_rkey = do
            fmap ResourceKey $ TP.many1 $ TP.satisfy (not . isSpace)

        p_entry = do
            bucket <- p_bucket
            _ <- TP.char ':'
            rkey <- p_rkey
            return $ (bucket, rkey)

        p_two_entries = do
            e1 <- p_entry
            TP.spaces
            e2 <- p_entry
            return (e1, e2)

        p_chgm = do
            e <- p_entry
            TP.spaces
            mime <- p_mime
            return $ ChangeMime e (C8.pack mime)

        p_list = do
            b <- p_bucket
            TP.spaces
            prefix <- p_maybe_quoted_s
            return $ List b prefix

        p_mime = p_maybe_quoted_s

        p_maybe_quoted_s = do
            c <- TP.lookAhead TP.anyChar
            if c == '"'
                then quoted_string
                else TP.many1 $ TP.satisfy (not . isSpace)

        lexer = TPT.makeTokenParser haskellDef
        quoted_string = TPT.stringLiteral lexer


cmdInCmdLineReader ::
#if MIN_VERSION_optparse_applicative(0, 11, 0)
    ReadM Command
cmdInCmdLineReader = do
    s <- str
#else
    (Monad m) => String -> m Command
cmdInCmdLineReader s = do
#endif
    (either (\e -> fail $ "failed to parse command: " ++ show e) return $
        TP.parse parseCommand "" s)
        >>= maybe
                (fail $ "failed to parse commands in cmdline: " ++ show s)
                return

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


printResult :: MonadIO m =>
    (a -> m ())
    -> WsResult a
    -> m ()
printResult f ws_result = do
    case ws_result of
        Left http_err -> do
            liftIO $ hPutStrLn stderr $ "HTTP Error: " ++ show http_err
        Right (Left err) -> do
            liftIO $ hPutStrLn stderr $ "Web Service Error: " ++ show err
        Right (Right x) -> do
            f x


processCmd ::
    (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey -> AccessKey -> Command -> m ()
processCmd secret_key access_key (Stat entry) = do
    (stat secret_key access_key entry) >>= printResult f
    where
        f = liftIO . print
processCmd secret_key access_key (Delete entry) = do
    (delete secret_key access_key entry) >>= printResult (const $ return ())
processCmd secret_key access_key (Copy entry_from entry_to) = do
    (copy secret_key access_key entry_from entry_to) >>= printResult (const $ return ())
processCmd secret_key access_key (Move entry_from entry_to) = do
    (move secret_key access_key entry_from entry_to) >>= printResult (const $ return ())
processCmd secret_key access_key (ChangeMime entry mime) = do
    (chgm secret_key access_key entry mime) >>= printResult (const $ return ())
processCmd secret_key access_key (List bucket prefix) = do
    (tryWsResult $ listSource secret_key access_key bucket 1 "/" prefix $$ CL.consume)
        >>= printResult f . unpackError
    where
        f s = liftIO $ do
                print s


interactive ::
    (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    SecretKey -> AccessKey -> m ()
interactive secret_key access_key = do
    liftIO $ hSetBuffering stdout NoBuffering
    err_ <- try go
    case err_ of
        Left err -> if isEOFError err
                        then return ()
                        else do
                            liftIO $ hPutStrLn stderr $ show err
                            return ()
        Right _  -> return ()

    where
        go = do
            liftIO $ putStr ">"
            line <- liftIO getLine
            if line == "quit" || line == "exit"
                then return ()
                else do
                    case TP.parse parseCommand "" line of
                        Left err -> liftIO $ hPutStrLn stderr $
                                        "cannot parse command: " ++ show err
                        Right Nothing -> return ()

                        Right (Just cmd) -> processCmd secret_key access_key cmd
                    go


start ::
    (MonadIO m, MonadReader Manager m, MonadCatch m) =>
    Maybe [Command] -> ReaderT ManageOptions m ()
start m_cmds = do
    mo <- ask
    let secret_key = moSecretKey mo
        access_key = moAccessKey mo
    lift $ do
        case m_cmds of
            Nothing -> interactive secret_key access_key
            Just cmds -> mapM_ (processCmd secret_key access_key) cmds

start' :: ManageOptions -> Maybe [Command] -> IO ()
start' mo cmds = do
    withManager defaultManagerSettings $ \mgmt -> do
        logger_set <- newStderrLoggerSet 0
        flip runReaderT mgmt $ do
            runLoggingT
                (runReaderT (start cmds) mo)
                (appLogger logger_set (moVerbose mo))


main :: IO ()
main = execParser opts >>= uncurry start'
    where
        opts = info (helper <*> ((,) <$> parseOptions
                                    <*> optional (some $ argument cmdInCmdLineReader $ metavar "COMMAND ...")))
                (fullDesc
                    <> progDesc "Management Tool for QiNiu"
                    <> header "qmanage - manage resources in QiNiu")
