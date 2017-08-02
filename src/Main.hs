{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (readFile, putStrLn)
import Control.Monad.Reader (Reader, runReader, ask)
import qualified Data.Map.Strict as M
import Data.ByteString (readFile)
import Data.Yaml (decode, decodeEither)
import Factorio.Data.Config
import Factorio.Data.Recipes
import Data.Text
import Data.Text.IO (putStrLn)

config_path :: FilePath
config_path = "./data/configuration.yaml"

recipe_book_path :: FilePath
recipe_book_path = "./data/recipes.15.31.yaml"

get_recipe_name :: RecipeAlias -> Reader Config (Maybe RecipeName)
get_recipe_name alias = do
  config <- ask
  return (M.lookup alias (aliases config))

get_recipe :: RecipeName -> Reader RecipeBook (Maybe Recipe)
get_recipe recipe_name = do
  recipe_book <- ask
  return (M.lookup recipe_name recipe_book)

main :: IO ()
main = do
  config_json <- readFile config_path
  recipe_book_json <- readFile recipe_book_path
  let recipe_book = decode recipe_book_json :: Maybe RecipeBook
  
  let result = recipe_book >>= (runReader (get_recipe "steam-engine"))
  putStrLn $ (pack . show) result