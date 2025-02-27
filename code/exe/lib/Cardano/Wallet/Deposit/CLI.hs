{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Shared types and helpers for CLI parsing
module Cardano.Wallet.Deposit.CLI
    ( LogOutput (..)
    , LoggingOptions
    , Mode (..)
    , cli
    , cmdVersion
    , databaseOption
    , depositByronGenesisFileOption
    , ekgEnabled
    , helperTracing
    , hostPreferenceOption
    , listenDepositOption
    , listenDepositUiOption
    , loggingMinSeverity
    , loggingOptions
    , loggingSeverityOrOffReader
    , loggingTracers
    , modeOption
    , runCli
    , setupDirectory
    , shutdownHandlerFlag
    , tlsOption
    , withLogging
    , networkConfigurationOption
    , nodeSocketOption
    , ServeArgs (..)
    ) where

import Prelude hiding
    ( getLine
    )

import Cardano.BM.Backend.Switchboard
    ( Switchboard
    )
import Cardano.BM.Configuration.Static
    ( defaultConfigStdout
    )
import Cardano.BM.Counters
    ( readCounters
    )
import Cardano.BM.Data.Configuration
    ( Endpoint (..)
    )
import Cardano.BM.Data.Counter
    ( Counter (..)
    , nameCounter
    )
import Cardano.BM.Data.LogItem
    ( LOContent (..)
    , LoggerName
    , PrivacyAnnotation (..)
    , mkLOMeta
    )
import Cardano.BM.Data.Output
    ( ScribeDefinition (..)
    , ScribeFormat (..)
    , ScribeId
    , ScribeKind (..)
    , ScribePrivacy (..)
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.SubTrace
    ( SubTrace (..)
    )
import Cardano.BM.Setup
    ( setupTrace_
    , shutdown
    )
import Cardano.BM.Trace
    ( Trace
    , appendName
    , logDebug
    , traceNamedObject
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , cardanoNodeConn
    , isWindows
    )
import Cardano.Wallet.Application.Server
    ( Listen (..)
    )
import Cardano.Wallet.Application.Tls
    ( TlsConfiguration (..)
    )
import Cardano.Wallet.Application.Tracers (TracerSeverities)
import Cardano.Wallet.Application.Version
    ( gitRevision
    , showFullVersion
    , version
    )
import Cardano.Wallet.Network.Config (NetworkConfiguration (..))
import Cardano.Wallet.Orphans
    (
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Control.Applicative
    ( optional
    , (<|>)
    )
import Control.Arrow
    ( left
    )
import Control.Monad
    ( forM_
    , forever
    , join
    , unless
    , when
    )
import Control.Monad.IO.Class
    ( MonadIO
    )
import Data.Bifunctor
    ( Bifunctor (..)
    , bimap
    )
import Data.Char
    ( toLower
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Streaming.Network
    ( HostPreference
    )
import Data.String
    ( IsString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , showT
    )
import Data.Text.Read
    ( decimal
    )
import Data.Void
    ( Void
    )
import GHC.Generics
    ( Generic
    )
import GHC.TypeLits
    ( Symbol
    )
import Options.Applicative
    ( CommandFields
    , Mod
    , OptionFields
    , ParseError (InfoMsg)
    , Parser
    , ParserInfo
    , abortOption
    , auto
    , command
    , customExecParser
    , eitherReader
    , flag'
    , header
    , help
    , helper
    , hidden
    , info
    , long
    , metavar
    , option
    , prefs
    , progDesc
    , showDefaultWith
    , showHelpOnEmpty
    , str
    , subparser
    , switch
    , value
    )
import Options.Applicative.Types
    ( ReadM (..)
    , readerAsk
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    )
import System.Environment
    ( lookupEnv
    )
import System.Exit
    ( exitFailure
    , exitSuccess
    )
import System.IO
    ( stderr
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( bracket
    )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.BM.Data.Observable as Obs
import qualified Data.Bifunctor as Bi
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified UnliftIO.Async as Async

{-------------------------------------------------------------------------------
                                   CLI
-------------------------------------------------------------------------------}

-- | Construct a CLI from a list of a commands
--
-- >>> runCli $ cli $ cmdA <> cmdB <> cmdC
cli :: Mod CommandFields a -> ParserInfo a
cli cmds =
    info (helper <*> subparser cmds)
        $ progDesc "Cardano Wallet Command-Line Interface (CLI)"
            <> header
                ( mconcat
                    [ "The CLI is a proxy to the wallet server, which is required for most "
                    , "commands. Commands are turned into corresponding API calls, and "
                    , "submitted to an up-and-running server. Some commands do not require "
                    , "an active server and can be run offline (e.g. 'recovery-phrase generate')."
                    ]
                )

-- | Runs a specific command parser using appropriate preferences
runCli :: ParserInfo (IO ()) -> IO ()
runCli = join . customExecParser preferences
  where
    preferences = prefs showHelpOnEmpty

{-------------------------------------------------------------------------------
                              Options & Arguments
-------------------------------------------------------------------------------}

-- | --database=DIR
databaseOption :: Parser FilePath
databaseOption =
    optionT
        $ long "database"
            <> metavar "DIR"
            <> help
                "use this directory for storing wallets. Run in-memory otherwise."

-- | [--listen-address=HOSTSPEC], default: 127.0.0.1
hostPreferenceOption :: Parser HostPreference
hostPreferenceOption =
    option str
        $ long "listen-address"
            <> metavar "HOST"
            <> help
                ( "Specification of which host to bind the API server to. "
                    <> "Can be an IPv[46] address, hostname, or '*'."
                )
            <> value "127.0.0.1"
            <> showDefaultWith (const "127.0.0.1")

-- | [--deposit-random-port|--deposit-port=INT]
listenDepositOption :: Parser (Maybe Listen)
listenDepositOption =
    (Just ListenOnRandomPort <$ depositRandomPortOption)
        <|> (Just . ListenOnPort . getPort <$> depositPortOption)
        <|> pure Nothing

-- | [--ui-deposit-random-port|--ui-deposit-port=INT]
listenDepositUiOption :: Parser (Maybe Listen)
listenDepositUiOption =
    (Just ListenOnRandomPort <$ uiDepositRandomPortOption)
        <|> (Just . ListenOnPort . getPort <$> uiDepositPortOption)
        <|> pure Nothing

-- | [--deposit-byron-genesis-file=FILEPATH]
depositByronGenesisFileOption :: Parser FilePath
depositByronGenesisFileOption =
    option str
        $ long "deposit-byron-genesis-file"
            <> metavar "FILEPATH"
            <> help "Byron genesis file to use for the deposit wallet."

-- | [--ui-deposit-random-port]
uiDepositRandomPortOption :: Parser Bool
uiDepositRandomPortOption =
    flag' False
        $ long "ui-deposit-random-port"
            <> help
                "serve the deposit wallet UI on any available port (conflicts with --ui-deposit-port)"

-- | [--ui-deposit-port=INT]
uiDepositPortOption :: Parser (Port "Wallet UI")
uiDepositPortOption =
    optionT
        $ long "ui-deposit-port"
            <> metavar "INT"
            <> help "port used for serving the deposit wallet UI."
            <> showDefaultWith showT

-- | [--deposit-random-port]
depositRandomPortOption :: Parser Bool
depositRandomPortOption =
    flag' False
        $ long "deposit-random-port"
            <> help
                "serve deposit wallet API on any available port (conflicts with --deposit-port)"

-- | [--deposit-port=INT]
depositPortOption :: Parser (Port "Deposit Wallet")
depositPortOption =
    optionT
        $ long "deposit-port"
            <> metavar "INT"
            <> help "port used for serving the deposit wallet JSON API."
            <> showDefaultWith showT

-- | [--shutdown-handler]
shutdownHandlerFlag :: Parser Bool
shutdownHandlerFlag =
    switch
        ( long "shutdown-handler"
            <> help "Enable the clean shutdown handler (exits when stdin is closed)"
        )

-- | --sync-tolerance=DURATION, default: 300s
syncToleranceOption :: Parser SyncTolerance
syncToleranceOption =
    optionT
        $ long "sync-tolerance"
            <> metavar "DURATION"
            <> help
                ( mconcat
                    [ "time duration within which we consider being synced with the "
                    , "network. Expressed in seconds with a trailing 's'."
                    ]
                )
            <> value fiveMinutes
            <> showDefaultWith showT
  where
    fiveMinutes = SyncTolerance (5 * 60)

-- | The lower-case names of all 'Severity' values.
loggingSeverities :: [(String, Severity)]
loggingSeverities = [(toLower <$> show s, s) | s <- [minBound .. maxBound]]

parseLoggingSeverity :: String -> Either String Severity
parseLoggingSeverity arg =
    case lookup (map toLower arg) loggingSeverities of
        Just sev -> pure sev
        Nothing -> Left $ "unknown logging severity: " ++ arg

loggingSeverityReader :: ReadM Severity
loggingSeverityReader = eitherReader parseLoggingSeverity

loggingSeverityOrOffReader :: ReadM (Maybe Severity)
loggingSeverityOrOffReader = do
    arg <- readerAsk
    case map toLower arg of
        "off" -> pure Nothing
        _ -> Just <$> loggingSeverityReader
tlsOption
    :: Parser TlsConfiguration
tlsOption =
    TlsConfiguration
        <$> tlsCaCertOption
        <*> tlsSvCertOption
        <*> tlsSvKeyOption
  where
    tlsCaCertOption =
        optionT
            $ long "tls-ca-cert"
                <> metavar "FILE"
                <> help "A x.509 Certificate Authority (CA) certificate."

    tlsSvCertOption =
        optionT
            $ long "tls-sv-cert"
                <> metavar "FILE"
                <> help "A x.509 Server (SV) certificate."

    tlsSvKeyOption =
        optionT
            $ long "tls-sv-key"
                <> metavar "FILE"
                <> help "The RSA Server key which signed the x.509 server certificate."

-- | Helper for writing an option 'Parser' using a 'FromText' instance.
optionT :: FromText a => Mod OptionFields a -> Parser a
optionT = option (eitherReader fromTextS)

-- | Like 'fromText', but stringly-typed.
fromTextS :: FromText a => String -> Either String a
fromTextS = left getTextDecodingError . fromText . T.pack

{-------------------------------------------------------------------------------
                                Extra Types
-------------------------------------------------------------------------------}

-- | Port number with a tag for describing what it is used for
newtype Port (tag :: Symbol) = Port {getPort :: Int}
    deriving stock (Eq, Generic)
    deriving newtype (Enum, Ord, Show)

-- NOTE
-- TCP port ranges from [[-1;65535]] \ {0}
-- However, ports in [[-1; 1023]] \ {0} are well-known ports reserved
-- and only "bindable" through root privileges.
instance Bounded (Port tag) where
    minBound = Port 1_024
    maxBound = Port 65_535

instance FromText (Port tag) where
    fromText t = do
        (p, unconsumed) <- bimap (const err) (first Port) (decimal t)
        unless (T.null unconsumed && p >= minBound && p <= maxBound)
            $ Left err
        return p
      where
        err =
            TextDecodingError
                $ "expected a TCP port number between "
                    <> show (getPort minBound)
                    <> " and "
                    <> show (getPort maxBound)

instance ToText (Port tag) where
    toText (Port p) = toText p

-- | Wrapper type around 'Text' to make its semantic more explicit
newtype Service = Service Text deriving newtype (IsString, Show, Eq)

newtype TxId = TxId {getTxId :: Hash "Tx"}
    deriving (Eq, Show)

instance FromText TxId where
    fromText = Bi.first (const err) . fmap TxId . fromText
      where
        err =
            TextDecodingError
                "A transaction ID should be a hex-encoded string of 64 characters."

{-------------------------------------------------------------------------------
                                  Logging
-------------------------------------------------------------------------------}

-- | Controls how much information to include in log output.
data Verbosity
    = -- | The default level of verbosity.
      Default
    | -- | Include less information in the log output.
      Quiet
    | -- | Include more information in the log output.
      Verbose
    deriving (Eq, Show)

data LogOutput
    = -- | Log to console, with the given minimum 'Severity'.
      --
      -- Logs of Warning or higher severity will be output to stderr. Notice or
      -- lower severity logs will be output to stdout.
      LogToStdStreams Severity
    | LogToFile FilePath Severity
    deriving (Eq, Show)

mkScribe :: LogOutput -> [ScribeDefinition]
mkScribe (LogToFile path sev) =
    pure
        $ ScribeDefinition
            { scName = T.pack path
            , scFormat = ScText
            , scKind = FileSK
            , scMinSev = sev
            , scMaxSev = Critical
            , scPrivacy = ScPublic
            , scRotation = Nothing
            }
mkScribe (LogToStdStreams sev) =
    [ mkScribe' (max errMin sev, maxBound, StderrSK)
    , mkScribe' (sev, pred errMin, StdoutSK)
    ]
  where
    errMin = Warning
    mkScribe' (minSev, maxSev, kind) =
        ScribeDefinition
            { scName = "text"
            , scFormat = ScText
            , scKind = kind
            , scMinSev = minSev
            , scMaxSev = maxSev
            , scPrivacy = ScPublic
            , scRotation = Nothing
            }

mkScribeId :: LogOutput -> [ScribeId]
mkScribeId (LogToStdStreams _) = ["StdoutSK::text", "StderrSK::text"]
mkScribeId (LogToFile file _) = pure $ T.pack $ "FileSK::" <> file

getPrometheusURL :: IO (Maybe (String, Port "Prometheus"))
getPrometheusURL = do
    prometheus_port <- lookupEnv "CARDANO_WALLET_PROMETHEUS_PORT"
    prometheus_host <-
        fromMaybe "127.0.0.1" <$> lookupEnv "CARDANO_WALLET_PROMETHEUS_HOST"
    case (prometheus_host, prometheus_port) of
        (host, Just port) ->
            case fromText @(Port "Prometheus") $ T.pack port of
                Right port' -> pure $ Just (host, port')
                _ -> do
                    TIO.hPutStr
                        stderr
                        "Port value for prometheus metrics invalid. Will be disabled."
                    pure Nothing
        _ -> pure Nothing

getEKGURL :: IO (Maybe (String, Port "EKG"))
getEKGURL = do
    ekg_port <- lookupEnv "CARDANO_WALLET_EKG_PORT"
    ekg_host <-
        fromMaybe "127.0.0.1" <$> lookupEnv "CARDANO_WALLET_EKG_HOST"
    case (ekg_host, ekg_port) of
        (host, Just port) ->
            case fromText @(Port "EKG") $ T.pack port of
                Right port' -> pure $ Just (host, port')
                _ -> do
                    TIO.hPutStr
                        stderr
                        "Port value for EKB metrics invalid. Will be disabled."
                    pure Nothing
        _ -> pure Nothing

ekgEnabled :: IO Bool
ekgEnabled = isJust <$> getEKGURL

-- | Initialize logging at the specified minimum 'Severity' level.
initTracer
    :: LoggerName
    -> [LogOutput]
    -> IO (Switchboard Text, (CM.Configuration, Trace IO Text))
initTracer loggerName outputs = do
    prometheusHP <- getPrometheusURL
    ekgHP <- getEKGURL
    cfg <- do
        c <- defaultConfigStdout
        CM.setSetupBackends
            c
            [CM.KatipBK, CM.AggregationBK, CM.EKGViewBK, CM.EditorBK]
        CM.setDefaultBackends c [CM.KatipBK]
        CM.setSetupScribes c $ outputs >>= mkScribe
        CM.setDefaultScribes c $ outputs >>= mkScribeId
        CM.setBackends c "test-cluster.metrics" (Just [CM.EKGViewBK])
        CM.setBackends c "cardano-wallet.metrics" (Just [CM.EKGViewBK])
        forM_ ekgHP $ \(h, p) -> do
            CM.setEKGBindAddr c $ Just (Endpoint (h, getPort p))
        forM_ prometheusHP $ \(h, p) ->
            CM.setPrometheusBindAddr c $ Just (h, getPort p)
        pure c
    (tr, sb) <- setupTrace_ cfg loggerName
    ekgEnabled >>= flip when (startCapturingMetrics tr)
    pure (sb, (cfg, tr))
  where
    -- https://github.com/IntersectMBO/cardano-node/blob/f7d57e30c47028ba2aeb306a4f21b47bb41dec01/cardano-node/src/Cardano/Node/Configuration/Logging.hs#L224
    startCapturingMetrics :: Trace IO Text -> IO ()
    startCapturingMetrics trace0 = do
        let trace = appendName "metrics" trace0
            counters =
                [ Obs.MemoryStats
                , Obs.ProcessStats
                , Obs.NetStats
                , Obs.IOStats
                , Obs.GhcRtsStats
                , Obs.SysStats
                ]
        _ <- Async.async $ forever $ do
            cts <- readCounters (ObservableTraceSelf counters)
            traceCounters trace cts
            threadDelay 30_000_000 -- 30 seconds
        pure ()
      where
        traceCounters
            :: forall m a. MonadIO m => Trace m a -> [Counter] -> m ()
        traceCounters _tr [] = return ()
        traceCounters tr (c@(Counter _ct cn cv) : cs) = do
            mle <- mkLOMeta Notice Confidential
            traceNamedObject tr (mle, LogValue (nameCounter c <> "." <> cn) cv)
            traceCounters tr cs

-- | See 'withLoggingNamed'
withLogging
    :: [LogOutput]
    -> ((Switchboard Text, (CM.Configuration, Trace IO Text)) -> IO a)
    -> IO a
withLogging =
    withLoggingNamed "cardano-wallet"

-- | Run an action with logging available and configured. When the action is
-- finished (normally or otherwise), log messages are flushed.
withLoggingNamed
    :: LoggerName
    -> [LogOutput]
    -> ((Switchboard Text, (CM.Configuration, Trace IO Text)) -> IO a)
    -- ^ The action to run with logging configured.
    -> IO a
withLoggingNamed loggerName outputs = bracket before after
  where
    before = initTracer loggerName outputs
    after (sb, (_, tr)) = do
        logDebug (appendName "main" tr) "Logging shutdown."
        shutdown sb

data LoggingOptions tracers = LoggingOptions
    { loggingMinSeverity :: Severity
    , loggingTracers :: tracers
    , loggingTracersDoc :: Maybe Void
    }
    deriving (Show, Eq)

loggingOptions :: Parser tracers -> Parser (LoggingOptions tracers)
loggingOptions tracers =
    LoggingOptions
        <$> minSev
        <*> tracers
        <*> tracersDoc
  where
    -- Note: If the global log level is Info then there will be no Debug-level
    --   messages whatsoever.
    --   If the global log level is Debug then there will be Debug, Info, and
    --   higher-severity messages.
    --   So the default global log level is Debug.
    minSev =
        option loggingSeverityReader
            $ long "log-level"
                <> value Debug
                <> metavar "SEVERITY"
                <> help
                    "Global minimum severity for a message to be logged. \
                    \Individual tracers severities still need to be configured \
                    \independently. Defaults to \"DEBUG\"."
                <> hidden
    tracersDoc =
        optional
            $ option auto
            $ long "trace-NAME"
                <> metavar "SEVERITY"
                <> help
                    "Individual component severity for 'NAME'. See --help-tracing \
                    \for details and available tracers."

-- | A hidden "helper" option which always fails, but shows info about the
-- logging options.
helperTracing :: [(String, String)] -> Parser (a -> a)
helperTracing tracerDescriptions =
    abortOption (InfoMsg helpTxt)
        $ long "help-tracing"
            <> help "Show help for tracing options"
            <> hidden
  where
    helpTxt = helperTracingText tracerDescriptions

helperTracingText :: [(String, String)] -> String
helperTracingText tracerDescriptions =
    unlines
        $ [ "Additional tracing options:"
          , ""
          , "  --log-level SEVERITY     Global minimum severity for a message to be logged."
          , "                           Defaults to \"DEBUG\"."
          , ""
          , "  --trace-NAME=off         Disable logging on the given tracer."
          , "  --trace-NAME=SEVERITY    Minimum severity for a message to be logged, or"
          , "                           \"off\" to disable the tracer. Note that component"
          , "                           traces still abide by the global log-level. For"
          , "                           example, if the global log level is \"INFO\", then"
          , "                           there will be no \"DEBUG\" messages whatsoever."
          , "                           Defaults to \"INFO\"."
          , ""
          , "The possible log levels (lowest to highest) are:"
          , "  " ++ unwords (map fst loggingSeverities)
          , ""
          , "The possible tracers are:"
          ]
            ++ [pretty_ tracerName desc | (tracerName, desc) <- tracerDescriptions]
  where
    maxLength = maximum $ map (length . fst) tracerDescriptions
    pretty_ tracerName desc =
        "  " ++ padRight maxLength ' ' tracerName ++ "  " ++ desc
      where
        padRight n c cs = take n $ cs ++ replicate n c

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

-- | Like 'hPutErrLn' but with provided default 'Handle'
putErrLn :: Text -> IO ()
putErrLn = TIO.hPutStrLn stderr

data Mode c = Normal c SyncTolerance
    deriving (Show)

modeOption :: Parser c -> Parser (Mode c)
modeOption nodeSocketOption' = normalMode
  where
    normalMode =
        Normal <$> nodeSocketOption' <*> syncToleranceOption

{-------------------------------------------------------------------------------
                            Commands - 'version'
-------------------------------------------------------------------------------}

cmdVersion :: Mod CommandFields (IO ())
cmdVersion =
    command "version"
        $ info cmd
        $ progDesc "Show the program's version."
  where
    cmd = pure exec
    exec = do
        putStrLn $ showFullVersion version gitRevision
        exitSuccess

{-------------------------------------------------------------------------------
                            Commands - 'launch'
-------------------------------------------------------------------------------}

-- | Initialize a directory to store data such as blocks or the wallet databases
setupDirectory :: (Text -> IO ()) -> FilePath -> IO ()
setupDirectory logT dir = do
    exists <- doesFileExist dir
    when exists $ do
        putErrLn
            $ mconcat
                [ T.pack dir <> " must be a directory, but it is"
                , " a file. Exiting."
                ]
        exitFailure
    doesDirectoryExist dir >>= \case
        True -> logT $ "Using directory: " <> T.pack dir
        False -> do
            logT $ "Creating directory: " <> T.pack dir
            let createParentIfMissing = True
            createDirectoryIfMissing createParentIfMissing dir

-- | --mainnet --shelley-genesis=FILE
-- --testnet --byron-genesis=FILE --shelley-genesis=FILE
networkConfigurationOption :: Parser NetworkConfiguration
networkConfigurationOption = mainnet <|> testnet
  where
    mainnet = mainnetFlag
    testnet = TestnetConfig <$> genesisFileOption "byron" "testnet"

    mainnetFlag =
        flag' MainnetConfig
            $ long "mainnet" <> help "Use Cardano mainnet protocol"

    genesisFileOption :: String -> String -> Parser FilePath
    genesisFileOption era net =
        option (eitherReader $ first getTextDecodingError . fromText . T.pack)
            $ long net
                <> metavar "FILE"
                <> help ("Path to the " <> era <> " genesis data in JSON format.")

-- | --node-socket=FILE
nodeSocketOption :: Parser CardanoNodeConn
nodeSocketOption =
    option (eitherReader (addHelp . cardanoNodeConn))
        $ long "node-socket"
            <> metavar (if isWindows then "PIPENAME" else "FILE")
            <> help helpText
  where
    helpText =
        mconcat
            [ "Path to the node's domain socket file (POSIX) "
            , "or pipe name (Windows). "
            , "Note: Maximum length for POSIX socket files is approx. 100 bytes. "
            , "Note:"
            , pipeHelp
            ]
    pipeHelp = " Windows named pipes are of the form \\\\.\\pipe\\cardano-node"
    addHelp = first (if isWindows then (++ pipeHelp) else id)

-- | Arguments for the 'serve' command
data ServeArgs = ServeArgs
    { _hostPreference :: HostPreference
    , _mode :: Mode CardanoNodeConn
    , _listenDeposit :: Maybe Listen
    , _listenDepositUi :: Maybe Listen
    , _tlsConfig :: Maybe TlsConfiguration
    , _networkConfiguration :: NetworkConfiguration
    , _database :: Maybe FilePath
    , _enableShutdownHandler :: Bool
    , _logging :: LoggingOptions TracerSeverities
    , _depositByronGenesisFile :: Maybe FilePath
    }
    deriving (Show)
