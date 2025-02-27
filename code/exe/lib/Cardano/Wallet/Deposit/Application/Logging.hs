{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Deposit.Application.Logging
where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Extra
    ( transformTextTrace
    )
import Cardano.BM.Plugin
    ( loadPlugin
    )
import Cardano.BM.Trace
    ( Trace
    , appendName
    , logInfo
    , logNotice
    )
import Cardano.Startup
    ( ShutdownHandlerLog
    , installSignalHandlers
    )
import Cardano.Wallet.Application.Tracers as Tracers
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , setupTracers
    , tracerLabels
    )
import Cardano.Wallet.Application.Version
    ( GitRevision
    , Version
    , showFullVersion
    )
import Cardano.Wallet.Deposit.CLI
    ( LogOutput (..)
    , LoggingOptions
    , ServeArgs
    , ekgEnabled
    , loggingMinSeverity
    , loggingSeverityOrOffReader
    , loggingTracers
    , withLogging
    )
import Control.Applicative
    ( Const (..)
    )
import Control.Exception.Base
    ( AsyncException (..)
    )
import Control.Monad
    ( when
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Options.Applicative
    ( Parser
    , internal
    , long
    , metavar
    , option
    , value
    )
import Servant (URI)
import System.Environment
    ( getArgs
    , getExecutablePath
    )
import UnliftIO.Exception
    ( withException
    )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.Wallet.Application.Version as V
import qualified Data.Text as T
import qualified System.Info as I

data MainLog
    = MsgCmdLine String [String]
    | MsgVersion Version GitRevision String String
    | MsgSetupStateDir Text
    | MsgSetupDatabases Text
    | MsgServeArgs ServeArgs
    | MsgListenAddress URI
    | MsgSigTerm
    | MsgSigInt
    | MsgShutdownHandler ShutdownHandlerLog
    | MsgFailedToParseGenesis Text
    deriving (Show)

instance ToText MainLog where
    toText = \case
        MsgCmdLine exe args ->
            T.pack $ unwords ("Command line:" : exe : args)
        MsgVersion ver rev arch os ->
            "Running as "
                <> T.pack (showFullVersion ver rev)
                <> " on "
                <> T.pack arch
                <> "-"
                <> T.pack os
        MsgSetupStateDir txt ->
            "Wallet state: " <> txt
        MsgSetupDatabases txt ->
            "Wallet databases: " <> txt
        MsgServeArgs args ->
            T.pack $ show args
        MsgListenAddress url ->
            "Wallet backend server listening on " <> T.pack (show url)
        MsgSigTerm ->
            "Terminated by signal."
        MsgSigInt ->
            "Interrupted by user."
        MsgShutdownHandler msg' ->
            toText msg'
        MsgFailedToParseGenesis hint ->
            T.unwords
                [ "Failed to parse Byron genesis configuration. You may want to check"
                , "the filepath given via --genesis and make sure it points to a "
                , "valid JSON Byron genesis file. The genesis file must be Byron, not"
                , "Shelley as it used to feed the wallet with the initial blockchain"
                , "parameters."
                , "Here's (perhaps) some helpful hint:"
                , hint
                ]

withTracers
    :: LoggingOptions TracerSeverities
    -> (Trace IO MainLog -> Tracers IO -> IO a)
    -> IO a
withTracers logOpt action =
    withLogging [LogToStdStreams (loggingMinSeverity logOpt)] $ \(sb, (cfg, tr)) -> do
        ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)
        let trMain = appendName "main" (transformTextTrace tr)
        let tracers = setupTracers (loggingTracers logOpt) tr
        logInfo trMain $ MsgVersion V.version V.gitRevision I.arch I.os
        logInfo trMain =<< MsgCmdLine <$> getExecutablePath <*> getArgs
        installSignalHandlers (logNotice trMain MsgSigTerm)
        let logInterrupt UserInterrupt = logNotice trMain MsgSigInt
            logInterrupt _ = pure ()
        action trMain tracers `withException` logInterrupt

tracerSeveritiesOption :: Parser TracerSeverities
tracerSeveritiesOption =
    Tracers
        <$> traceOpt applicationTracer (Just Info)
        <*> traceOpt apiServerTracer (Just Info)
        <*> traceOpt tokenMetadataTracer (Just Info)
        <*> traceOpt walletEngineTracer (Just Info)
        <*> traceOpt walletDbTracer (Just Info)
        <*> traceOpt poolsEngineTracer (Just Info)
        <*> traceOpt poolsDbTracer (Just Info)
        <*> traceOpt ntpClientTracer (Just Info)
        <*> traceOpt networkTracer (Just Info)
  where
    traceOpt field def =
        fmap Const . option loggingSeverityOrOffReader
            $ long ("trace-" <> T.unpack (getConst (field tracerLabels)))
                <> value def
                <> metavar "SEVERITY"
                <> internal
