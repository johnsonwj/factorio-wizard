{-# LANGUAGE OverloadedStrings #-}

module Main where

import Factorio.Data.Env
import Factorio.Factory

import Control.Monad.Reader (Reader, runReader, ask)
import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Text.IO as T
import Data.Yaml (prettyPrintParseException)

configPath :: FilePath
configPath = "./data/configuration.yaml"

recipeBookPath :: FilePath
recipeBookPath = "./data/recipes.15.31.yaml"

main :: IO ()
main = do
  env <- getEnv configPath recipeBookPath
  case env of
    Left ex -> putStrLn $ prettyPrintParseException ex
    Right (c, _) -> T.putStrLn (version c)
