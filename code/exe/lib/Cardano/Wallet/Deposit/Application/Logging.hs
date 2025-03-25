{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Application.Logging
where

import Prelude

import Cardano.BM.Backend.Switchboard (Switchboard)
import Cardano.BM.Counters (readCounters)
import Cardano.BM.Data.Configuration (Endpoint (..))
import Cardano.BM.Data.Counter (Counter (..), nameCounter)
import Cardano.BM.Data.LogItem
    ( LOContent (LogValue)
    , LoggerName
    , PrivacyAnnotation (Confidential)
    , mkLOMeta
    )
import Cardano.BM.Data.Output
    ( ScribeDefinition (..)
    , ScribeFormat (ScText)
    , ScribeId
    , ScribeKind (FileSK, StderrSK, StdoutSK)
    , ScribePrivacy (ScPublic)
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.SubTrace (SubTrace (ObservableTraceSelf))
import Cardano.BM.Extra
    ( transformTextTrace
    )
import Cardano.BM.Plugin
    ( loadPlugin
    )
import Cardano.BM.Setup (setupTrace_, shutdown)
import Cardano.BM.Trace
    ( Trace
    , appendName
    , logDebug
    , logInfo
    , logNotice
    , traceNamedObject
    )
import Cardano.BM.Tracing (defaultConfigStdout)
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
import Cardano.Wallet.Deposit.Application.Options
    ( LoggingOptions
    , Port
    , ServeArgs
    , getPort
    , loggingMinSeverity
    , loggingSeverityOrOffReader
    , loggingTracers
    )
import Control.Applicative
    ( Const (..)
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Base
    ( AsyncException (..)
    )
import Control.Monad
    ( forM_
    , forever
    , when
    )
import Data.Maybe (fromMaybe, isJust)
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
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
    , lookupEnv
    )
import UnliftIO (MonadIO, bracket, stderr, withException)

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Cardano.BM.Configuration as CM
import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.Backend as CM
import qualified Cardano.BM.Data.Observable as Obs
import qualified Cardano.Wallet.Application.Version as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Info as I
import qualified UnliftIO.Async as Async

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
    | MsgDebug String
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
        MsgDebug txt ->
            T.pack txt

data DepositTracers = DepositTracers
    { walletTracers :: Tracers IO
    , depositTrace :: Trace IO MainLog
    }

withTracers
    :: LoggingOptions TracerSeverities
    -> (DepositTracers -> IO a)
    -> IO a
withTracers logOpt action =
    withLogging [LogToStdStreams (loggingMinSeverity logOpt)]
        $ \(sb, (cfg, tr)) -> do
            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)
            let trMain = appendName "main" (transformTextTrace tr)
            let tracers = setupTracers (loggingTracers logOpt) tr
            logInfo trMain $ MsgVersion V.version V.gitRevision I.arch I.os
            logInfo trMain =<< MsgCmdLine <$> getExecutablePath <*> getArgs
            installSignalHandlers (logNotice trMain MsgSigTerm)
            let logInterrupt UserInterrupt = logNotice trMain MsgSigInt
                logInterrupt _ = pure ()
                depositTracers = DepositTracers tracers trMain
            action depositTracers `withException` logInterrupt

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

data LogOutput
    = -- | Log to console, with the given minimum 'Severity'.
      --
      -- Logs of Warning or higher severity will be output to stderr. Notice or
      -- lower severity logs will be output to stdout.
      LogToStdStreams Severity
    | LogToFile FilePath Severity
    deriving (Eq, Show)

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
