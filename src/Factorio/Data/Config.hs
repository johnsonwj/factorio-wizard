{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Factorio.Data.Config where

import Control.Applicative
import Data.Map.Strict (Map)
import Data.Text
import Data.Yaml (FromJSON(..))
import Data.Aeson.Types (Options, defaultOptions, camelTo2, fieldLabelModifier, genericParseJSON)
import GHC.Generics

type RecipeAlias = Text
type RecipeName = Text
type OutpostName = Text
type ScienceType = Text

type OnsiteRecipes = [RecipeName]
type StockpilePhase = Map RecipeName Int
type OutpostPlan = Map OutpostName [StockpilePhase]

type FurnaceName = Text
type FuelName = Text
type MineableName = Text
type AssemblerName = Text

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '-' }

data AdditionalData = AdditionalData { smeltingBurnerConsumption :: Map FurnaceName Float
                                     , fuelValues :: Map FuelName Float
                                     , smeltingSpeed :: Map FurnaceName Float
                                     , miningSpeed :: Map MineableName Float
                                     , craftingSpeed :: Map AssemblerName Float
                                     } deriving (Show, Generic)

instance FromJSON AdditionalData where
  parseJSON = genericParseJSON jsonOptions

data Config = Config { version :: Text
                     , labCount :: Int
                     , stockpilePeriod :: Int
                     , aliases :: Map RecipeAlias RecipeName
                     , science :: [ScienceType]
                     , onsite :: OnsiteRecipes
                     , outposts :: OutpostPlan
                     , additionalData :: AdditionalData
                     } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON jsonOptions
                    