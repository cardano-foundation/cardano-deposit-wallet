{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

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
import Cardano.Wallet.Deposit.CLI
    ( Mode (..)
    , ServeArgs (..)
    , databaseOption
    , depositByronGenesisFileOption
    , helperTracing
    , hostPreferenceOption
    , listenDepositOption
    , listenDepositUiOption
    , loggingOptions
    , modeOption
    , networkConfigurationOption
    , nodeSocketOption
    , setupDirectory
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
import System.Exit
    ( ExitCode (..)
    , exitWith
    )
import "optparse-applicative" Options.Applicative
    ( CommandFields
    , Mod
    , command
    , helper
    , info
    , progDesc
    )

import qualified Data.Text as T

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
