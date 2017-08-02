import yaml
import sys

clean_recipes = dict()

def transform_ingredients_list(ingredients):
  new_ingredients = dict()

  for ing in ingredients:

    try:
      (ing_name, ing_count) = ing
    except ValueError as ve:
      ing_name = ing['name']
      ing_count = ing['amount']

    new_ingredients[ing_name] = ing_count
  
  return new_ingredients

with open('data/recipes.15.31_dumb.yaml') as yf:
  recipes = yaml.safe_load(yf)

  for (recipe_name, recipe) in recipes.items():
    clean_recipe = dict()
    
    for prop in ['category', 'energy_required', 'result_count']:
      if prop in recipe:
        clean_recipe[prop] = recipe[prop]

    if 'ingredients' in recipe:
      ingredients_list = recipe['ingredients']
    elif 'normal' in recipe and 'ingredients' in recipe['normal']:
      ingredients_list = recipe['normal']['ingredients']
    else:
      print('malformed recipe! {}'.format(recipe_name))
      sys.exit(1)

    clean_recipe['ingredients'] = transform_ingredients_list(ingredients_list)

    clean_recipes[recipe_name] = clean_recipe

with open('data/recipes.15.31.yaml', 'w') as cyf:
  yaml.dump(clean_recipes, cyf, default_flow_style=False)
