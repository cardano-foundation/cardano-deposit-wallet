{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Deposit.Application.Commands.Serve
    ( cmdServe
    ) where

import Prelude

import Cardano.BM.Extra
    ( trMessage
    )
import Cardano.BM.Trace
    ( Trace
    , logDebug
    , logError
    , logInfo
    )
import Cardano.Startup
    ( withShutdownHandler
    )
import Cardano.Wallet.Application.Tracers as Tracers
    ( Tracers' (..)
    , tracerDescriptions
    )
import Cardano.Wallet.Deposit.Application
    ( serveDepositWallet
    )
import Cardano.Wallet.Deposit.Application.Logging
    ( MainLog (..)
    , tracerSeveritiesOption
    , withTracers
    )
import Cardano.Wallet.Deposit.Application.Options
    ( Mode (..)
    , ServeArgs (..)
    , databaseOption
    , depositByronGenesisFileOption
    , hostPreferenceOption
    , listenDepositOption
    , listenDepositUiOption
    , loggingOptions
    , loggingSeverities
    , modeOption
    , networkConfigurationOption
    , nodeSocketOption
    , shutdownHandlerFlag
    , tlsOption
    )
import Cardano.Wallet.Network.Config
    ( parseGenesisData
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( tunedForMainnetPipeliningStrategy
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer
    )
import Control.Applicative
    ( optional
    )
import Control.Monad
    ( void
    , when
    , (<=<)
    )
import Control.Monad.Trans.Cont
    ( evalContT
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
    ( forM_
    )
import Data.Functor.Contravariant (Contravariant (..))
import Data.Text (Text)
import Options.Applicative
    ( CommandFields
    , Mod
    , ParseError (InfoMsg)
    , Parser
    , abortOption
    , command
    , help
    , helper
    , hidden
    , info
    , long
    , progDesc
    )
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    )
import System.Exit
    ( ExitCode (..)
    , exitFailure
    , exitWith
    )
import System.IO (stderr)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

cmdServe :: Mod CommandFields (IO ())
cmdServe =
    command "serve"
        $ info (helper <*> helper' <*> cmd)
        $ progDesc "Serve API that listens for commands/actions."
  where
    helper' = helperTracing tracerDescriptions

    cmd =
        fmap exec
            $ ServeArgs
                <$> hostPreferenceOption
                <*> modeOption nodeSocketOption
                <*> listenDepositOption
                <*> listenDepositUiOption
                <*> optional tlsOption
                <*> networkConfigurationOption
                <*> optional databaseOption
                <*> shutdownHandlerFlag
                <*> loggingOptions tracerSeveritiesOption
                <*> optional depositByronGenesisFileOption

exec :: ServeArgs -> IO ()
exec
    args@( ServeArgs
                host
                mode
                listenDeposit
                listenDepositUi
                tlsConfig
                networkConfig
                databaseDir
                enableShutdownHandler
                logOpt
                byronGenesisFileOpt
            ) = withTracers logOpt $ \tr tracers -> do
        withShutdownHandlerMaybe tr enableShutdownHandler $ do
            logDebug tr $ MsgServeArgs args

            (discriminant, netParams, vData, _block0) <-
                runExceptT (parseGenesisData networkConfig) >>= \case
                    Right x -> pure x
                    Left err -> do
                        logError tr (MsgFailedToParseGenesis $ T.pack err)
                        exitWith $ ExitFailure 33
            forM_ databaseDir
                $ setupDirectory (logInfo tr . MsgSetupDatabases)
            blockchainSource <- case mode of
                Normal conn syncTolerance ->
                    pure $ NodeSource conn vData syncTolerance
            exitWith
                <=< evalContT
                $ do
                    netLayer <-
                        withNetworkLayer
                            (networkTracer tracers)
                            tunedForMainnetPipeliningStrategy
                            blockchainSource
                            discriminant
                            netParams
                    serveDepositWallet
                        tracers
                        databaseDir
                        host
                        listenDeposit
                        listenDepositUi
                        tlsConfig
                        byronGenesisFileOpt
                        netLayer

withShutdownHandlerMaybe :: Trace IO MainLog -> Bool -> IO () -> IO ()
withShutdownHandlerMaybe _ False = void
withShutdownHandlerMaybe tr True = void . withShutdownHandler trShutdown
  where
    trShutdown = trMessage $ contramap (second (fmap MsgShutdownHandler)) tr

-- | A hidden "helper" option which always fails, but shows info about the
-- logging options.
helperTracing :: [(String, String)] -> Parser (a -> a)
helperTracing tracerDescriptions' =
    abortOption (InfoMsg helpTxt)
        $ long "help-tracing"
            <> help "Show help for tracing options"
            <> hidden
  where
    helpTxt = helperTracingText tracerDescriptions'

helperTracingText :: [(String, String)] -> String
helperTracingText tracerDescriptions' =
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
            ++ [pretty_ tracerName desc | (tracerName, desc) <- tracerDescriptions']
  where
    maxLength = maximum $ map (length . fst) tracerDescriptions'
    pretty_ tracerName desc =
        "  " ++ padRight maxLength ' ' tracerName ++ "  " ++ desc
      where
        padRight n c cs = take n $ cs ++ replicate n c

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

-- | Like 'hPutErrLn' but with provided default 'Handle'
putErrLn :: Text -> IO ()
putErrLn = TIO.hPutStrLn stderr
