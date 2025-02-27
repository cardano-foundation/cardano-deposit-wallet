{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Shared types and helpers for CLI parsing
module Cardano.Wallet.Deposit.Application.Options
    ( LoggingOptions
    , Mode (..)
    , Port (..)
    , databaseOption
    , depositByronGenesisFileOption
    , hostPreferenceOption
    , listenDepositOption
    , listenDepositUiOption
    , loggingMinSeverity
    , loggingOptions
    , loggingSeverityOrOffReader
    , loggingTracers
    , loggingSeverities
    , modeOption
    , shutdownHandlerFlag
    , tlsOption
    , networkConfigurationOption
    , nodeSocketOption
    , ServeArgs (..)
    ) where

import Prelude hiding
    ( getLine
    )

import Cardano.BM.Data.Severity
    ( Severity (..)
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
import Cardano.Wallet.Network.Config (NetworkConfiguration (..))
import Cardano.Wallet.Orphans
    (
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..)
    )
import Control.Applicative
    ( optional
    , (<|>)
    )
import Control.Arrow
    ( left
    )
import Control.Monad
    ( unless
    )
import Data.Bifunctor
    ( Bifunctor (..)
    , bimap
    )
import Data.Char
    ( toLower
    )
import Data.Streaming.Network
    ( HostPreference
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
    ( Mod
    , OptionFields
    , Parser
    , auto
    , eitherReader
    , flag'
    , help
    , hidden
    , long
    , metavar
    , option
    , showDefaultWith
    , str
    , switch
    , value
    )
import Options.Applicative.Types
    ( ReadM (..)
    , readerAsk
    )

import qualified Data.Text as T

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

{-------------------------------------------------------------------------------
                                  Logging
-------------------------------------------------------------------------------}
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

data Mode c = Normal c SyncTolerance
    deriving (Show)

modeOption :: Parser c -> Parser (Mode c)
modeOption nodeSocketOption' = normalMode
  where
    normalMode =
        Normal <$> nodeSocketOption' <*> syncToleranceOption

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
