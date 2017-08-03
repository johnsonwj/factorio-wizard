{-# LANGUAGE OverloadedStrings #-}

module Factorio.Factory ( planFactory
                        , ProductionSchedule
                        , Outpost
                        , FactoryUpgrade
                        , Factory
                        , FactoryPlan
                        ) where

import Factorio.Data.Env

import Control.Monad.Reader (Reader, ask)
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

(?:) :: Maybe a -> a -> a
(Just x)  ?: y = x
Nothing   ?: y = y

type ProductionSchedule = (Map RecipeName Float)

instance Monoid ProductionSchedule where
  mempty = M.empty
  mappend s1 s2 =
    let foldRecipes recipe rate newSchedule = M.insert recipe newRate newSchedule
          where newRate = (newSchedule !? recipe) ?: 0.0 + rate
    in M.foldrWithKey foldRecipes s1 s2

type Outpost = (Int, ProductionSchedule)
type OutpostPhase = (OutpostName, Int, ProductionSchedule)

{-
stockpileToRate :: Int -> StockpilePhase -> ProductionSchedule
stockpileToRate stockpilePeriod = M.map toRate
    where toRate s = toIntegral s / 60.0 * toIntegral stockpilePeriod

flattenOutpostPlan :: Int -> OutpostPlan -> [OutpostPhase]
flattenOutpostPlan stockpilePeriod =
  let phaseStockpiles stockpiles =
        zip [1..] (map (stockpileToRate stockpilePeriod) stockpiles)
      planFolder oname stockpiles outposts =
        outposts >< (oname, )
  -}
  in Seq.toList $ Map.foldrWithKey planFolder Seq.empty

type Factory = Map OutpostName Outpost

                    -- OutpostUpgrade (name) (delta)
data FactoryUpgrade = OutpostUpgrade OutpostName ProductionSchedule
                    | ScienceUpgrade ScienceType

-- these versions do not include transitive dependencies from onsite recipes
recipeDependencies' :: RecipeBook -> RecipeName -> [RecipeName]
recipeDependencies' book name = (M.keys . ingredients) (book ! name)

ingredientsAvailable' :: RecipeBook -> ProductDirectory -> RecipeName -> Bool
ingredientsAvailable' rb products recipe =
  all flip elem (M.keys products) (recipeDependencies' rb recipe)

-- recipeDependencies :: Env -> RecipeName -> [RecipeName]
    -- determine which recipes this one depends on, mapping onsite recipes to their sub-ingredients

-- ingredientsAvailable :: Env -> ProductDirectory -> RecipeName -> Bool
    -- determine if the given recipe can be made with ingredients in the current product directory
    -- aware of onsite recipes

dealiasScience :: Config -> ScienceType -> RecipeName
dealiasScience cfg = (!) (aliases cfg) . (++ "-science")

scienceRecipes :: Env -> [Recipe]
scienceRecipes (cfg, rb) = do
  sciType <- science cfg
  return (rb ! dealiasScience sciType)

-- addScience :: Env -> FactoryPlan -> FactoryPlan
    -- insert each ScienceUpgrade in between FactoryUpgrades once that science recipe can be made

type ProductDirectory = Map RecipeName OutpostName

getProductDirectory :: Factory -> ProductDirectory
getProductDirectory =
  let dirFragment outpostName = M.map (const outpostName)
      foldProducts outpostName (_, output) = M.union (dirFragment outpostName output)
  in M.foldrWithKey foldProducts M.empty

-- 

runUpgrades :: [FactoryUpgrade] -> Factory
runUpgrades =
  let newOutpost factory (oname, newProduction)
        | M.member oname factory  =
            let (oldPhase, oldProduction) = factory ! oname
            in Outpost (oldPhase + 1) (oldProduction <> newProduction)
        | otherwise               = Outpost 0 newProduction

      foldUpgrade factory upgrade = M.insert oname (newOutpost factory upgrade) factory

  in foldl foldUpgrade M.empty 

type FactoryPlan = (Factory, [FactoryUpgrade])

planFactory :: Reader Env FactoryPlan
planFactory = do
  (cfg, rb) <- ask
  let upgrades = planUpgrades cfg rb
  let finalFactory = runUpgrades upgrades
  return (finalFactory, upgrades)
