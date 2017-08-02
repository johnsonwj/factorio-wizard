{-# LANGUAGE OverloadedStrings #-}

module Factorio.Data.Recipes where

import Data.Map.Strict (Map)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=), withObject, withText)
import qualified Data.Yaml as Y
import Factorio.Data.Config
import Data.Text

data RecipeCategory = Standard | Smelting deriving (Show)

instance FromJSON RecipeCategory where
  parseJSON = withText "Category" $ \v ->
    case v of
      "smelting" -> return Smelting
      _          -> return Standard

data Recipe = Recipe { category :: RecipeCategory
                     , time :: Float
                     , ingredients :: Map RecipeName Int
                     , resultCount :: Int
                     } deriving (Show)
  
instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \v -> Recipe
    <$> v .:? "category" .!= Standard
    <*> v .:? "energy_required" .!= 0.5
    <*> v .: "ingredients"
    <*> v .:? "result_count" .!= 1

type RecipeBook = Map RecipeName Recipe
