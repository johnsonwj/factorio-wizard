{-# LANGUAGE OverloadedStrings #-}

module Factorio.Data.Config where

import Data.Map.Strict (Map)
import Data.Yaml (FromJSON(..), (.:), withObject)
import Control.Applicative
import Data.Text

type RecipeAlias = Text
type RecipeName = Text
type OutpostName = Text
type ScienceType = Text

type StockpilePhase = Map RecipeName Int
type OutpostPlan = Map OutpostName [StockpilePhase]

type FurnaceName = Text
type FuelName = Text
type MineableName = Text
type AssemblerName = Text

data AdditionalData = AdditionalData { smeltingBurnerConsumption :: Map FurnaceName Float
                                     , fuelValues :: Map FuelName Float
                                     , smeltingSpeed :: Map FurnaceName Float
                                     , miningSpeed :: Map MineableName Float
                                     , craftingSpeed :: Map AssemblerName Float
                                     } deriving (Show)

instance FromJSON AdditionalData where
  parseJSON = withObject "AdditionalData" $ \v -> AdditionalData
    <$> v .: "smelting-burner-consumption"
    <*> v .: "fuel-values"
    <*> v .: "smelting-speed"
    <*> v .: "mining-speed"
    <*> v .: "crafting-speed"

data Config = Config { version :: Text
                     , labCount :: Int
                     , stockpilePeriod :: Int
                     , aliases :: Map RecipeAlias RecipeName
                     , science :: [ScienceType]
                     , onsite :: [RecipeName]
                     , outposts :: OutpostPlan
                     , additionalData :: AdditionalData
                     } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "version"
    <*> v .: "lab-count"
    <*> v .: "stockpile-period"
    <*> v .: "aliases"
    <*> v .: "science"
    <*> v .: "onsite"
    <*> v .: "outposts"
    <*> v .: "additional-data"
                     