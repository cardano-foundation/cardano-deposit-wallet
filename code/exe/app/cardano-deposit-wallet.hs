module Main where

import Prelude

import Cardano.Wallet.Deposit.Application.Commands.Serve (cmdServe)
import Cardano.Wallet.Deposit.Application.Commands.Version
    ( cmdVersion
    )
import Cardano.Wallet.Deposit.Application.Options
    ( cli
    , runCli
    )
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $ runCli $ cli $ cmdServe <> cmdVersion
