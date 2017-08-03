module Factorio.Data.Env
  ( module Factorio.Data.Config
  , module Factorio.Data.Recipes
  , Env
  , getEnv
  ) where

import Factorio.Data.Config
import Factorio.Data.Recipes

import Data.Yaml (decodeFileEither, ParseException)

type Env = (Config, RecipeBook)

getEnv :: FilePath -> FilePath -> IO (Either ParseException Env)
getEnv configPath recipeBookPath = do
  config_result <- decodeFileEither configPath
  recipe_book_result <- decodeFileEither recipeBookPath

  return $ do
    config <- config_result
    recipe_book <- recipe_book_result
    return  (config, recipe_book)
