module Factorio.Data.Env
  ( module Factorio.Data.Config
  , module Factorio.Data.Recipes
  , Env
  , get_env
  ) where

import Factorio.Data.Config
import Factorio.Data.Recipes

import Data.Yaml (decodeFileEither, ParseException)

type Env = (Config, RecipeBook)

get_env :: FilePath -> FilePath -> IO (Either ParseException Env)
get_env config_path recipe_book_path = do
  config_result <- decodeFileEither config_path
  recipe_book_result <- decodeFileEither recipe_book_path

  return $ do
    config <- config_result
    recipe_book <- recipe_book_result
    return  (config, recipe_book)
