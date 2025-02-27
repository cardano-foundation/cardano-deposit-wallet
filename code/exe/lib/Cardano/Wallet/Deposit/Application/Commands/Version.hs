-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Shared types and helpers for CLI parsing
module Cardano.Wallet.Deposit.Application.Commands.Version
    ( cmdVersion
    ) where

import Prelude hiding
    ( getLine
    )

import Cardano.Wallet.Application.Version
    ( gitRevision
    , showFullVersion
    , version
    )
import Cardano.Wallet.Orphans
    (
    )
import Options.Applicative
    ( CommandFields
    , Mod
    , command
    , info
    , progDesc
    )
import System.Exit
    ( exitSuccess
    )

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
