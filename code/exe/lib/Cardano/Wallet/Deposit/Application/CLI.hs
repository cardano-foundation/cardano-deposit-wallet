module Cardano.Wallet.Deposit.Application.CLI
    ( cli
    , runCli
    ) where

import Prelude hiding
    ( getLine
    )

import Cardano.Wallet.Orphans
    (
    )
import Control.Monad
    ( join
    )
import Options.Applicative
    ( CommandFields
    , Mod
    , ParserInfo
    , customExecParser
    , header
    , helper
    , info
    , prefs
    , progDesc
    , showHelpOnEmpty
    , subparser
    )

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
