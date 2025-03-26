{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubToBytes
    )
import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.REST
    ( ErrDatabase
    )
import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Copy (copyButton)
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxDelete_
    , hxGet_
    , hxInclude_
    , hxPost_
    , hxSwap_
    , hxTarget_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , dataBsDismiss_
    , linkText
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( ModalData (..)
    , mkModal
    , mkModalButton
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.API
    ( walletDeleteLink
    , walletDeleteModalLink
    , walletLink
    , walletMnemonicLink
    , walletPostMnemonicLink
    , walletPostXPubLink
    , walletStatusLink
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( chainPointToSlotH
    , networkTagH
    , timeH
    , valueH
    , withOriginH
    )
import Cardano.Wallet.UI.Deposit.Types.Wallet
    ( Status (..)
    )
import Control.Exception
    ( SomeException
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Attribute
    , Html
    , HtmlT
    , ToHtml (..)
    )
import Lucid.Html5
    ( autocomplete_
    , button_
    , class_
    , div_
    , id_
    , input_
    , name_
    , p_
    , placeholder_
    , role_
    , type_
    )
import Servant (Link)

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

data WalletPresent
    = WalletPresent WalletPublicIdentity
    | WalletAbsent
    | WalletFailedToInitialize ErrDatabase
    | WalletVanished SomeException
    | WalletInitializing
    | WalletClosing

isPresent :: WalletPresent -> Bool
isPresent = \case
    WalletPresent _ -> True
    _ -> False
instance Show WalletPresent where
    show (WalletPresent x) = "WalletPresent: " <> show x
    show WalletAbsent = "WalletAbsent"
    show (WalletFailedToInitialize _) = "WalletFailedToInitialize"
    show (WalletVanished _) = "WalletVanished"
    show WalletInitializing = "WalletInitializing"
    show WalletClosing = "WalletClosing"

walletH :: Monad m => HtmlT m ()
walletH = sseH walletLink "wallet" ["wallet"]

pubKeyH :: Monad m => XPub -> HtmlT m ()
pubKeyH xpub =
    truncatableText WithCopy "public_key"
        $ toHtml
        $ B16.encode
        $ xpubToBytes xpub

headAndTail :: Int -> ByteString -> ByteString
headAndTail n t = B8.take n t <> " .. " <> B8.takeEnd n t

deleteWalletButtonH :: Html ()
deleteWalletButtonH =
    mkModalButton
        walletDeleteModalLink
        [class_ "btn btn-danger"]
        "Delete Wallet"

deleteWalletModalH :: Html ()
deleteWalletModalH =
    mkModal
        $ ModalData
            { modalTitle = "Delete Wallet"
            , modalBody = p_ "Are you sure you want to delete this wallet?"
            , modalFooter = do
                button_
                    [ class_ "btn btn-danger"
                    , hxDelete_ $ linkText walletDeleteLink
                    , dataBsDismiss_ "modal"
                    , hxSwap_ "none"
                    ]
                    "Delete Wallet"
                button_
                    [ class_ "btn btn-secondary"
                    , dataBsDismiss_ "modal"
                    ]
                    "Cancel"
            }

walletStatusH :: Status -> Html ()
walletStatusH status = do
    box "Status" mempty
        $ record (Just 13) Full Striped
        $ do
            simpleField "Tip Slot" $ do
                chainPointToSlotH $ tip status
            simpleField "Tip Time" $ do
                maybe mempty (withOriginH timeH) (tipTime status)
            simpleField "Balance" $ valueH $ balance status
            simpleField "Network" $ networkTagH $ network status

walletElementH
    :: (BL.ByteString -> Html ())
    -> WalletPresent
    -> Html ()
walletElementH alert presence = case presence of
    WalletPresent (WalletPublicIdentity xpub customers) -> do
        div_ [class_ "row mt-2 gx-0"]
            $ sseH walletStatusLink "wallet-status" ["wallet-tip"]
        div_ [class_ "row mt-2 gx-0"] $ do
            box "Public Identity" mempty
                $ record (Just 13) Full Striped
                $ do
                    simpleField "Extended Public Key" $ pubKeyH xpub
                    simpleField "Tracked Addresses"
                        $ div_ [class_ "d-flex justify-content-end align-items-center"]
                        $ toHtml
                        $ toText customers
        div_ [class_ "row mt-2 gx-0"] $ do
            box "Management" mempty
                $ div_
                    [class_ "d-flex justify-content-end align-items-center"]
                    deleteWalletButtonH
            div_ [id_ "delete-result"] mempty
    WalletAbsent -> do
        div_ [class_ "row mt-2 gx-0"]
            $ newWalletFromMnemonicH
            $ PostWalletConfig
                { walletDataLink = walletPostMnemonicLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        div_ [class_ "row mt-2 gx-0"]
            $ newWalletFromXPubH
            $ PostWalletConfig
                { walletDataLink = walletPostXPubLink
                , passwordVisibility = Just Hidden
                , responseTarget = "#post-response"
                }
        div_ [class_ "row mt-2 gx-0"]
            $ div_ [id_ "post-response"] mempty
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
    WalletClosing -> alert "Wallet is closing"

onWalletPresentH
    :: (WalletPublicIdentity -> Html ())
    -> (BL.ByteString -> Html ())
    -> WalletPresent
    -> Html ()
onWalletPresentH f alert = \case
    WalletPresent wpi -> f wpi
    WalletAbsent -> alert "Wallet is absent"
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
    WalletClosing -> alert "Wallet is closing"

data BadgeStyle
    = Primary
    | Secondary
    | Success
    | Danger
    | Warning
    | Info
    | Light
    | Dark

renderBadgeStyle :: BadgeStyle -> Text
renderBadgeStyle = \case
    Primary -> "primary"
    Secondary -> "secondary"
    Success -> "success"
    Danger -> "danger"
    Warning -> "warning"
    Info -> "info"
    Light -> "light"
    Dark -> "dark"

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the wallet creation form
data PostWalletConfig = PostWalletConfig
    { passwordVisibility :: Maybe Visible
    -- ^ Whether the password should be visible
    , walletDataLink :: Link
    -- ^ Link to post the form data to
    , responseTarget :: Text
    }

--------------------------------------------------------------------------------
-- Library
--------------------------------------------------------------------------------

-- | Add a form tag with the appropriate attributes for a POST request
postWalletFormTagH
    :: Monad m
    => Text
    -- ^ ID of the form
    -> PostWalletConfig
    -- ^ Configuration for the form
    -> HtmlT m ()
    -> HtmlT m ()
postWalletFormTagH idName PostWalletConfig{} =
    div_
        [ autocomplete_ "off"
        , class_ "p-2"
        , id_ idName
        ]

--------------------------------------------------------------------------------
-- Wallet creation forms
--------------------------------------------------------------------------------

-- | Widget to create a new wallet from a mnemonic
newWalletFromMnemonicH :: Monad m => PostWalletConfig -> HtmlT m ()
newWalletFromMnemonicH config = do
    box "Restore from Mnemonic" mempty $ do
        div_ [class_ "p-2"] $ do
            postWalletFormTagH "wallet-from-menmonic" config
                $ mnemonicSetupFieldsH config

-- | Display a mnemonic
mnemonicH :: Maybe [Text] -> Html ()
mnemonicH Nothing = ""
mnemonicH (Just mnemonic) = do
    div_ [class_ "d-flex justify-content-end align-items-center"] $ do
        div_
            [ class_ "p-2"
            , id_ "copy-mnemonic"
            ]
            $ toHtml
            $ T.intercalate " " mnemonic
        copyButton "copy-mnemonic"

-- | Form fields for restoring a wallet from a mnemonic
mnemonicSetupFieldsH :: Monad m => PostWalletConfig -> HtmlT m ()
mnemonicSetupFieldsH PostWalletConfig{..} = do
    div_ [class_ "border-start"] $ do
        div_ [class_ "d-flex justify-content-end align-items-center"]
            $ div_ [class_ "btn-group", role_ "group"]
            $ do
                button_
                    [ class_ "btn btn-outline-secondary"
                    , hxGet_ $ linkText $ walletMnemonicLink $ Just False
                    , hxTarget_ "#menmonic"
                    ]
                    "Hint"
                button_
                    [ class_ "btn btn-outline-secondary"
                    , hxGet_ $ linkText $ walletMnemonicLink $ Just True
                    , hxTarget_ "#menmonic"
                    ]
                    "Clean"
        div_ [id_ "menmonic", class_ "mb"] ""
        input_
            [ formControl
            , visibility
            , name_ "mnemonics"
            , placeholder_ "Mnemonic Sentence"
            ]
    div_ [class_ "border-start"]
        $ input_
            [ formControl
            , type_ "number"
            , name_ "trackedCustomers"
            , placeholder_ "Tracked Customers"
            ]
    input_
        [ formControl
        , type_ "password"
        , name_ "password"
        , placeholder_ "Signing Passphrase"
        ]
    div_ [class_ "d-flex justify-content-end align-items-center"] $ do
        button_
            [ class_ "btn btn-primary btn-block mb-3"
            , type_ "submit"
            , hxPost_ $ linkText walletDataLink
            , hxTarget_ responseTarget
            , hxInclude_ "#wallet-from-menmonic"
            ]
            "Restore"
  where
    visibility = type_ $ case passwordVisibility of
        Just Visible -> "text"
        Just Hidden -> "password"
        Nothing -> "password"

--------------------------------------------------------------------------------
-- Wallet restoration from public key
--------------------------------------------------------------------------------

newWalletFromXPubH :: Monad m => PostWalletConfig -> HtmlT m ()
newWalletFromXPubH config@PostWalletConfig{..} = do
    box "Restore from Public Key" mempty $ div_ [class_ "p-2"] $ do
        postWalletFormTagH "wallet-from-xpub" config $ do
            input_
                [ formControl
                , type_ "text"
                , name_ "xpub"
                , placeholder_ "Extended Public Key"
                ]
            input_
                [ formControl
                , type_ "number"
                , name_ "trackedCustomers"
                , placeholder_ "Tracked Customers"
                ]
            div_ [class_ "d-flex justify-content-end align-items-center"] $ do
                button_
                    [ class_ "btn btn-primary btn-block mb-3"
                    , hxPost_ $ linkText walletDataLink
                    , hxTarget_ responseTarget
                    , hxInclude_ "#wallet-from-xpub"
                    ]
                    "Restore"

formControl :: Attribute
formControl = class_ "form-control m-1 px-3 py-"
