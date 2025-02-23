{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deposit.Application
    ( serveDepositWallet
    ) where

import Prelude

import Cardano.Wallet.Application.Logging
    ( ApplicationLog (..)
    )
import Cardano.Wallet.Application.Server
    ( Listen
    , ListenError (..)
    , start
    , withListeningSocket
    )
import Cardano.Wallet.Application.Tls
    ( TlsConfiguration
    )
import Cardano.Wallet.Application.Tracers as Tracers
    ( Tracers
    , Tracers' (..)
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv
    , WalletInstance
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( Resource
    , withResource
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    )
import Cardano.Wallet.Deposit.REST.Start
    ( loadDepositWalletFromDisk
    , newBootEnv
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( CardanoBlock
    , StandardCrypto
    )
import Cardano.Wallet.UI.Common.Html.Pages.Template.Head
    ( PageConfig (..)
    )
import Cardano.Wallet.UI.Common.Layer
    ( Push (..)
    , UILayer
    , oobMessages
    , sourceOfNewTip
    , walletTipChanges
    )
import Control.Concurrent (threadDelay)
import Control.Monad
    ( forever
    , void
    )
import Control.Monad.Trans.Class
    ( lift
    )
import Control.Monad.Trans.Cont
    ( ContT (..)
    , callCC
    )
import Control.Tracer
    ( Tracer
    , nullTracer
    , traceWith
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Streaming.Network
    ( HostPreference
    )
import Network.Socket
    ( Socket
    )
import Servant
    ( Application
    , serve
    )
import System.Exit
    ( ExitCode (..)
    )
import UnliftIO
    ( MonadIO (..)
    , withAsync
    , withSystemTempDirectory
    )

import qualified Cardano.Wallet.Deposit.HTTP.Server as Deposit
import qualified Cardano.Wallet.Deposit.HTTP.Types.API as Deposit
import qualified Cardano.Wallet.Deposit.IO.Resource.Event as REST
import qualified Cardano.Wallet.Deposit.REST as REST
import qualified Cardano.Wallet.UI.Common.Layer as Ui
import qualified Cardano.Wallet.UI.Deposit.API as DepositUi
import qualified Cardano.Wallet.UI.Deposit.Server as DepositUi
import qualified Network.Wai.Handler.Warp as Warp

-- | The @cardano-deposit-wallet@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveDepositWallet
    :: Tracers IO
    -- ^ Logging config.
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> HostPreference
    -- ^ Which host to bind.
    -> Maybe Listen
    -- ^ Optional HTTP JSON data server port for the deposit wallet.
    -> Maybe Listen
    -- ^ Optional HTTP UI Server port for the deposit wallet.
    -> Maybe TlsConfiguration
    -- ^ An optional TLS configuration
    -> Maybe FilePath
    -- See also: 'Cardano.Wallet.Primitive.Ledger.Shelley#KnownNetwork'.
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -- ^ A network layer to interact with the blockchain.
    -> ContT r IO ExitCode
serveDepositWallet
    Tracers{applicationTracer, apiServerTracer}
    databaseDir
    hostPref
    mListenDeposit
    mListenDepositUi
    tlsConfig
    depositByronGenesisFile
    netLayer = do
        eDepositUiSocket <- binding hostPref mListenDepositUi
        eDepositSocket <- binding hostPref mListenDeposit
        bootEnv <- lift $ newBootEnv depositByronGenesisFile netLayer
        callCC $ \exit -> do
            mDepositDatabaseDirAndResource <- case eDepositUiSocket of
                Left err -> do
                    lift $ trace $ MsgServerStartupError err
                    void $ exit $ ExitFailure $ exitCodeApiServer err
                    pure Nothing
                Right ms -> do
                    case ms of
                        Nothing -> pure Nothing
                        Just (_port, socket) ->
                            bootUI
                                databaseDir
                                bootEnv
                                netLayer
                                applicationTracer
                                $ start
                                    Warp.defaultSettings
                                    apiServerTracer
                                    tlsConfig
                                    socket
            case eDepositSocket of
                Left err -> do
                    lift $ trace $ MsgServerStartupError err
                    void $ exit $ ExitFailure $ exitCodeApiServer err
                Right ms -> do
                    case ms of
                        Nothing -> pure ()
                        Just (_port, socket) -> do
                            void
                                $ bootApi
                                    mDepositDatabaseDirAndResource
                                    bootEnv
                                    applicationTracer
                                $ start
                                    Warp.defaultSettings
                                    apiServerTracer
                                    tlsConfig
                                    socket
            void $ liftIO $ forever $ threadDelay 1000000
            exit ExitSuccess
      where
        trace :: ApplicationLog -> IO ()
        trace = traceWith applicationTracer

bootApi
    :: Maybe (FilePath, Resource REST.ErrDatabase WalletInstance)
    -> WalletBootEnv IO
    -> Tracer IO ApplicationLog
    -> (Application -> IO ())
    -> ContT r IO ()
bootApi mDepositDatabaseDirAndResource bootEnv applicationTracer starter = do
    (databaseDir', resource) <-
        case mDepositDatabaseDirAndResource of
            Nothing -> do
                databaseDir' <-
                    ContT
                        $ withSystemTempDirectory
                            "deposit-wallet"
                resource <- ContT withResource
                liftIO
                    $ loadDepositWalletFromDisk
                        nullTracer
                        ( DepositApplicationLog
                            >$< applicationTracer
                        )
                        databaseDir'
                        bootEnv
                        resource
                pure (databaseDir', resource)
            Just (databaseDir', w) ->
                pure (databaseDir', w)
    let apiService =
            startDepositServer
                resource
                bootEnv
                databaseDir'
                applicationTracer
                starter
    ContT $ \k ->
        withAsync apiService $ \_ -> k ()
    pure ()

bootUI
    :: Maybe FilePath
    -> WalletBootEnv IO
    -> NetworkLayer IO block
    -> Tracer IO ApplicationLog
    -> (Application -> IO ())
    -> ContT
        r
        IO
        ( Maybe
            ( FilePath
            , Resource
                REST.ErrDatabase
                WalletInstance
            )
        )
bootUI databaseDir bootEnv netLayer applicationTracer starter = do
    databaseDir' <- case databaseDir of
        Nothing -> ContT $ withSystemTempDirectory "deposit-wallet"
        Just databaseDir' -> pure databaseDir'
    resource <- ContT withResource
    ui <- Ui.withUILayer 1 resource
    liftIO
        $ loadDepositWalletFromDisk
            (walletTipChanges >$< oobMessages ui)
            ( DepositApplicationLog
                >$< applicationTracer
            )
            databaseDir'
            bootEnv
            resource
    REST.onResourceChange
        ( \_ -> do
            traceWith (oobMessages ui)
                $ Push "wallet"
        )
        resource
    sourceOfNewTip netLayer ui
    let uiService =
            startDepositUiServer
                ui
                bootEnv
                databaseDir'
                applicationTracer
                starter
    ContT $ \k ->
        withAsync uiService $ \_ -> k ()
    pure $ Just (databaseDir', resource)

binding
    :: HostPreference
    -> Maybe Listen
    -> ContT r IO (Either ListenError (Maybe (Warp.Port, Socket)))
binding hostPref = \case
    Nothing -> pure $ Right Nothing
    Just listen ->
        fmap (fmap Just)
            $ ContT
            $ withListeningSocket hostPref listen

startDepositServer
    :: WalletResource
    -> WalletBootEnv IO
    -> FilePath
    -> Tracer IO ApplicationLog
    -> (Application -> IO ())
    -> IO ()
startDepositServer
    resource
    bootEnv
    databaseDir'
    tracer
    server =
        server
            $ serve (Proxy @Deposit.API)
            $ Deposit.server
                nullTracer
                (DepositApplicationLog >$< tracer)
                databaseDir'
                bootEnv
                resource

startDepositUiServer
    :: UILayer WalletResource
    -> WalletBootEnv IO
    -> FilePath
    -> Tracer IO ApplicationLog
    -> (Application -> IO ())
    -> IO ()
startDepositUiServer
    ui
    bootEnv
    databaseDir'
    tracer
    server =
        server
            $ serve (Proxy @DepositUi.UI)
            $ DepositUi.serveUI
                (walletTipChanges >$< oobMessages ui)
                (DepositUIApplicationLog >$< tracer)
                ui
                bootEnv
                databaseDir'
                (PageConfig "" "Deposit Cardano Wallet")

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13
