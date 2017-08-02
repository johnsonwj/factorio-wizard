{-# LANGUAGE OverloadedStrings #-}

module Factorio.Factory ( plan_factory
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

instance Monoid ProductionSchedule =
  mempty = M.empty
  mappend s1 s2 =
    let fold_recipes recipe rate new_schedule = M.insert recipe new_rate new_schedule
          where new_rate = (new_schedule !? recipe) ?: 0.0 + rate
    in M.foldrWithKey fold_recipes s1 s2

type Outpost = (Int, ProductionSchedule)
type OutpostPhase = (OutpostName, Int, ProductionSchedule)

{-
stockpile_to_rate :: Int -> StockpilePhase -> ProductionSchedule
stockpile_to_rate stockpile_period stockpiles = M.map to_rate stockpiles
    where to_rate s = (toIntegral s) / ((60.0) * (toIntegral stockpile_period))

flatten_outpost_plan :: Int -> OutpostPlan -> [OutpostPhase]
flatten_outpost_plan stockpile_period =
  let phase_stockpiles stockpiles =
        zip [1..] (map (stockpile_to_rate stockpile_period) stockpiles)
      plan_folder oname stockpiles outposts =
        outposts >< (oname, )
  in Seq.toList $ Map.foldrWithKey plan_folder Seq.empty
  -}

type Factory = Map OutpostName Outpost

                    -- OutpostUpgrade (name) (delta)
data FactoryUpgrade = OutpostUpgrade OutpostName ProductionSchedule
                    | ScienceUpgrade ScienceType

-- these versions do not include transitive dependencies from onsite recipes
recipe_dependencies' :: RecipeBook -> RecipeName -> [RecipeName]
recipe_dependencies' book name = (M.keys . ingredients) (book ! name)

ingredients_available' :: RecipeBook -> ProductDirectory -> RecipeName -> Bool
ingredients_available' rb products recipe =
  all ((flip elem) (M.keys products)) (recipe_dependencies' rb recipe)

-- recipe_dependencies :: Env -> RecipeName -> [RecipeName]
    -- determine which recipes this one depends on, mapping onsite recipes to their sub-ingredients

-- ingredients_available :: Env -> ProductDirectory -> RecipeName -> Bool
    -- determine if the given recipe can be made with ingredients in the current product directory
    -- aware of onsite recipes

dealias_science :: Config -> ScienceType -> RecipeName
dealias_science cfg sci_type = ((!) $ aliases cfg) . (++ "-science")

science_recipes :: Env -> [Recipe]
science_recipes (cfg, rb) = do
  sci_type <- (science cfg)
  return (rb ! (dealias_science sci_type))

-- add_science :: Env -> FactoryPlan -> FactoryPlan
    -- insert each ScienceUpgrade in between FactoryUpgrades once that science recipe can be made

type ProductDirectory = Map RecipeName OutpostName

get_product_directory :: Factory -> ProductDirectory
get_product_directory =
  let dir_fragment outpost_name schedule = M.map (const outpost_name) schedule
      fold_products outpost_name (_, output) = M.union (dir_fragment outpost_name $ output)
  in M.foldrWithKey fold_products M.empty

-- 

run_upgrades :: [FactoryUpgrade] -> Factory
run_upgrades =
  let new_outpost factory (oname, new_production)
        | M.member oname factory  = Outpost (old_phase + 1) (old_production <> new_production)
            where (old_phase, old_production) = factory ! oname
        | otherwise               = Outpost 0 new_production

      fold_upgrade factory upgrade = M.insert oname (new_outpost factory upgrade) factory

  in foldl fold_upgrade M.empty 

type FactoryPlan = (Factory, [FactoryUpgrade])

plan_factory :: Reader Env FactoryPlan
plan_factory = do
  (cfg, rb) <- ask
  let upgrades = plan_upgrades cfg rb
  let final_factory = run_upgrades upgrades
  return (final_factory, upgrades)
