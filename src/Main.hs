{-# LANGUAGE OverloadedStrings #-}

module Main where

import Factorio.Data.Env
import Factorio.Wizard.Factory

import Control.Monad.Reader (Reader, runReader, ask)
import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Text.IO as T
import Data.Yaml (prettyPrintParseException)

config_path :: FilePath
config_path = "./data/configuration.yaml"

recipe_book_path :: FilePath
recipe_book_path = "./data/recipes.15.31.yaml"

main :: IO ()
main = do
  env <- get_env config_path recipe_book_path
  case env of
    Left ex -> putStrLn $ prettyPrintParseException ex
    Right (c, _) -> T.putStrLn (version c)
