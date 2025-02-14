{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Prelude

import CMark (Node (..), NodeType (CODE_BLOCK), commonmarkToNode)
import Control.Monad (when)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Char (ord)
import Data.Csv.Parser qualified as Csv
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Data.Yaml (FromJSON)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt

-- | Recursively select all nodes of a certain type
selectNodes :: (Node -> Maybe a) -> Node -> [a]
selectNodes f node =
    (children node >>= selectNodes f)
        & maybe id (:) (f node)
  where
    children :: Node -> [Node]
    children (Node _ _ nodes) = nodes

mkCodeBlock :: NodeType -> Maybe CodeBlock
mkCodeBlock (CODE_BLOCK i t) = Just $ CodeBlock i' t
  where
    i' = case parseOnly parseInfoString (encodeUtf8 i) of
        Left _ -> Nothing
        Right x -> Just x
mkCodeBlock _ = Nothing

onCodeBlockType :: Node -> Maybe CodeBlock
onCodeBlockType (Node _ t _) = mkCodeBlock t

data CodeBlock = CodeBlock
    { info :: Maybe InfoString
    , code :: Text
    }
    deriving (Show)

data InfoString = InfoString
    { interpreter :: Text
    , keys :: [Text]
    }
    deriving (Show)

parseInfoString :: P.Parser InfoString
parseInfoString = do
    interpreter <- P.takeWhile1 (/= ' ') <&> decodeUtf8
    keys <- parseKeys
    return InfoString{..}

parseKeys :: P.Parser [Text]
parseKeys =
    Csv.record coma <&> fmap (T.strip . decodeUtf8) . V.toList
  where
    coma = fromIntegral (ord ',')

data OneFile = OneFile
    { path :: FilePath
    , matches :: [Text]
    }
    deriving (Show, Generic)

instance FromJSON OneFile

newtype Recipe = Recipe [OneFile]
    deriving (Show, Generic)

instance FromJSON Recipe

-- | Load a recipe as a YAML file
loadRecipe :: FilePath -> IO Recipe
loadRecipe path = do
    content <- readFile path
    case Yaml.decodeEither' (encodeUtf8 $ pack content) of
        Left err -> error $ show err
        Right x -> return x

data Options = Options
    { recipeFile :: FilePath
    , baseDir :: FilePath
    , enableLogging :: Bool
    , enableEchoing :: Bool
    }
    deriving (Show)

parseOptions :: Opt.Parser Options
parseOptions =
    Options
        <$> Opt.strOption
            ( Opt.long "recipe"
                <> Opt.short 'r'
                <> Opt.metavar "FILE"
                <> Opt.help "Recipe file"
            )
        <*> Opt.strOption
            ( Opt.long "base-dir"
                <> Opt.short 'd'
                <> Opt.metavar "DIR"
                <> Opt.help "Base directory"
            )
        <*> Opt.switch
            ( Opt.long "enable-logging"
                <> Opt.short 'l'
                <> Opt.help "Enable logging"
            )
        <*> Opt.switch
            ( Opt.long "enable-echoing"
                <> Opt.short 'e'
                <> Opt.help "Enable echoing"
            )

withEchoing :: Bool -> String
withEchoing True = "set -euox pipefail"
withEchoing False = "set -euo pipefail"

preamble :: Options -> IO ()
preamble Options{..} = do
    putStrLn "#!/usr/bin/env bash"
    putStrLn $ withEchoing enableEchoing
    putStrLn "\n\n"

main :: IO ()
main = do
    options@Options{..} <-
        Opt.execParser $ Opt.info parseOptions Opt.fullDesc
    Recipe files <- loadRecipe recipeFile
    preamble options
    forM_ files $ \OneFile{..} -> do
        content <- readFile (baseDir <> "/" <> path)
        let node = commonmarkToNode [] (pack content)
        forM_ matches $ \m -> do
            forM_ (selectNodes onCodeBlockType node) $ \CodeBlock{..} -> do
                when (m `elem` maybe [] keys info) $ do
                    when enableLogging
                        $ putStrLn
                        $ "echo 'Running "
                            <> path
                            <> "' "
                            <> show m
                    putStrLn $ T.unpack code
