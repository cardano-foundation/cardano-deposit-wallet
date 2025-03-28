{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Payments.PageSpec
    ( spec
    )
where

import Prelude

import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Deposit.IO
    ( WalletBootEnv (WalletBootEnv)
    , networkEnv
    )
import Cardano.Wallet.Deposit.IO.Network.Mock
    ( newNetworkEnvMock
    )
import Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv
    , mapBlock
    , postTx
    )
import Cardano.Wallet.Deposit.IO.Resource
    ( withResource
    )
import Cardano.Wallet.Deposit.Pure
    ( BIP32Path (..)
    , Credentials
    , DerivationType (..)
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( createMnemonicFromWords
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.Pure.State.Payment.Inspect
    ( InspectTx (..)
    )
import Cardano.Wallet.Deposit.REST
    ( ErrWalletResource (..)
    , WalletResourceM
    , customerAddress
    , initWallet
    , inspectTx
    , resolveCurrentEraTx
    , runWalletResourceM
    )
import Cardano.Wallet.Deposit.Write
    ( addTxOut
    , emptyTxBody
    , mkAda
    , mkTx
    , mkTxOut
    )
import Cardano.Wallet.UI.Deposit.API.Payments
    ( unsigned
    )
import Cardano.Wallet.UI.Deposit.Handlers.Payments.Transaction
    ( deserializeTransaction
    , mkPayment
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad.Except
    ( runExceptT
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( nullTracer
    )
import Data.Data
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Test.Aeson.GenericSpecs
    ( roundtripAndGoldenSpecs
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldNotBe
    )
import Test.QuickCheck
    ( Arbitrary
    , choose
    , oneof
    )
import Test.QuickCheck.Arbitrary
    ( Arbitrary (..)
    )

import qualified Cardano.Wallet.Deposit.Read as Read

fakeBootEnv :: IO (WalletBootEnv IO)
fakeBootEnv = do
    net <- mapBlock Read.EraValue <$> newNetworkEnvMock
    pure $ WalletBootEnv nullTracer Read.mockGenesisDataMainnet net

mnemonics :: Text
mnemonics =
    "vital minimum victory start lunch find city peanut shiver soft hedgehog artwork mushroom loud found"

seed :: SomeMnemonic
Right seed = createMnemonicFromWords mnemonics

credentials :: Credentials
credentials =
    credentialsFromMnemonics seed mempty

letItInitialize :: WalletResourceM ()
letItInitialize = liftIO $ threadDelay 100_000

onSuccess :: (Show e, MonadFail m) => Either e a -> (a -> m b) -> m b
onSuccess (Left e) _ = fail $ show e
onSuccess (Right a) f = f a

withWallet :: WalletResourceM a -> IO (Either ErrWalletResource a)
withWallet f = withResource $ runWalletResourceM f

withInitializedWallet
    :: WalletResourceM a
    -> IO (Either ErrWalletResource a)
withInitializedWallet f =
    withSystemTempDirectory "wallet-ui" $ \dir -> do
        bootEnv <- fakeBootEnv
        withWallet $ do
            initWallet nullTracer nullTracer bootEnv dir credentials 1
            letItInitialize
            fundTheWallet (networkEnv bootEnv)
            f

fundTheWallet :: NetworkEnv IO z -> WalletResourceM ()
fundTheWallet network = do
    Just address <- customerAddress 0
    let tx =
            mkTx
                $ fst
                $ addTxOut (mkTxOut address (mkAda 1_000_000)) emptyTxBody
    Right () <- liftIO $ postTx network tx
    pure ()

spec :: Spec
spec = do
    describe "payment" $ do
        it "can create a transaction with no receivers" $ do
            etx <- withInitializedWallet $ do
                etx <- runExceptT $ unsigned mkPayment mempty
                onSuccess etx $ \tx -> do
                    onSuccess (deserializeTransaction tx) $ \dtx -> do
                        tx' <- resolveCurrentEraTx dtx
                        inspectTx tx'
            onSuccess etx $ \InspectTx{..} -> do
                change `shouldNotBe` []
                ourInputs `shouldNotBe` []
                fee `shouldNotBe` 0
    describe "BIP32 input paths"
        $ roundtripAndGoldenSpecs (Proxy @BIP32Path)

instance Arbitrary DerivationType where
    arbitrary = oneof [pure Soft, pure Hardened]

instance Arbitrary BIP32Path where
    arbitrary = oneof [pure Root, segment]
      where
        segment = do
            path <- arbitrary
            derivation <- arbitrary
            index <- fromIntegral <$> choose (0 :: Int, 2 ^ (31 :: Int) - 1)
            pure $ Segment path derivation index
