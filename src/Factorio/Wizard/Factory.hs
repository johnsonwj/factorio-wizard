module Factorio.Wizard.Factory where

import Factorio.Data.Env

import Control.Monad.Reader (Reader, ask)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))

type ProductionSchedule = (Map RecipeName Float)

data Outpost = Outpost { phase :: Int, output :: ProductionSchedule }

type Factory = Map OutpostName Outpost
data FactoryUpgrade = FactoryUpgrade { outpostName :: OutpostName, newOutpost :: Outpost }

-- these versions do not include transitive dependencies from onsite recipes
recipe_dependencies' :: RecipeBook -> RecipeName -> [RecipeName]
recipe_dependencies' book name = (M.keys . ingredients) (book ! name)

ingredients_available' :: RecipeBook -> ProductDirectory -> RecipeName -> Bool
ingredients_available' rb products recipe = all ((flip elem) (M.keys products)) (recipe_dependencies' rb recipe)

{-
recipe_dependencies :: Env -> RecipeName -> [RecipeName]
recipe_dependencies (cfg, rb) name =
  let transitives = M.fromList (map (\os -> (os, recipe_dependencies' os)))
-}

type ProductDirectory = Map RecipeName OutpostName

get_product_directory :: Factory -> ProductDirectory
get_product_directory =
  let dir_fragment outpost_name schedule = M.map (const outpost_name) schedule
      folder outpost_name outpost = M.union (dir_fragment outpost_name $ output outpost)
  in M.foldrWithKey folder M.empty

{-
available_ingredients :: Env -> ProductDirectory -> [RecipeName]
get_product_directory (cfg, rb) products =
  let available_onsite = filter
-}
-- flatten_outposts :: OutpostPlan -> Reader Env [Outpost]


