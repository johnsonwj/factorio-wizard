import yaml
import math
from pprint import pprint

with open('data/configuration.yaml') as yconfig:
    config = yaml.safe_load(yconfig)

with open('data/recipes.15.31.yaml') as yrecipes:
    recipes = yaml.safe_load(yrecipes)

with open('data/additional-data.yaml') as ydata:
    additional_data = yaml.safe_load(ydata)


science_types = [
    ('red science', 'science-pack-1'),
    ('green science', 'science-pack-2'),
    ('blue science', 'science-pack-3'),
    ('black science', 'military-science-pack'),
    ('purple science', 'production-science-pack'),
    ('yellow science', 'high-tech-science-pack')
]


def ingredients(recipe_name):
    if recipe_name in recipes:
        return recipes[recipe_name]['ingredients']
    return dict()


def is_recipe_of_category(recipe_name, category):
    if recipe_name not in recipes:
        return False

    recipe = recipes[recipe_name]
    if 'category' in recipe:
        return recipe['category'] == category

    return False


def is_smelting_recipe(recipe_name):
    return is_recipe_of_category(recipe_name, 'smelting')


class Schedule:
    def __init__(self, items=None):
        if items is None:
            self._items = dict()
        else:
            self._items = items

    def __add__(self, other):
        total = Schedule(self._items.copy())

        for k, v in other.items():
            total.add(k, v)

        return total

    def __getitem__(self, item):
        return self._items[item]

    def __str__(self):
        return str(self._items)

    def __len__(self):
        return len(self._items)

    def add(self, k, v):
        if k in self._items:
            self._items[k] += v
        else:
            self._items[k] = v

    def items(self):
        return self._items.items()


def crafting_effort(recipe_name):
    if recipe_name not in recipes:
        return 0.0

    recipe = recipes[recipe_name]

    if 'energy_required' in recipe:
        return recipe['energy_required']

    return 0.5  # default


def result_count(recipe_name):
    if recipe_name not in recipes:
        return 1

    recipe = recipes[recipe_name]

    if 'result_count' in recipe:
        return recipe['result_count']

    return 1


smelting_consumption = additional_data['smelting-burner-consumption']
smelting_speed = additional_data['smelting-speed']


def smelting_cost(recipe_name, recipe_rate):
    if not is_smelting_recipe(recipe_name):
        return 0, Schedule()

    effective_rate = recipe_rate / result_count(recipe_name)

    fuel_rates = Schedule({fuel_name: effective_rate * smelting_consumption / (smelting_speed * fuel_energy * 1000.)
                           for fuel_name, fuel_energy in additional_data['fuel-values'].items()})

    smelters_required = int(math.ceil(effective_rate * crafting_effort(recipe_name) / smelting_speed))

    return smelters_required, fuel_rates


mining_speeds = additional_data['mining-speed']


def drills_required(ore_name, ore_rate):
    if ore_name not in mining_speeds:
        return 0

    return int(math.ceil(ore_rate / mining_speeds[ore_name]))


assembler2_crafting_speed = additional_data['crafting-speed']['assembling-machine-2']


def assembler2_required(recipe_name, recipe_rate):
    if recipe_name not in recipes or is_smelting_recipe(recipe_name):
        return 0

    effective_rate = recipe_rate / result_count(recipe_name)

    return int(math.ceil(effective_rate * crafting_effort(recipe_name) / assembler2_crafting_speed))


def build_dependency_tree(recipe_name, desired_rate):
    tree = dict()
    tree['name'] = recipe_name
    tree['ingredients'] = [build_dependency_tree(ingredient_name, ingredient_count * desired_rate)
                           for ingredient_name, ingredient_count in ingredients(recipe_name).items()]

    smelters, fuel_rate = smelting_cost(recipe_name, desired_rate)
    drills = drills_required(recipe_name, desired_rate)
    assemblers = assembler2_required(recipe_name, desired_rate)

    tree['schedule'] = Schedule({
        'smelters': smelters,
        'fuel rate': fuel_rate,
        'drills': drills,
        'assemblers': assemblers
    })

    total_schedule = Schedule()
    total_schedule += tree['schedule']
    for ing_tree in tree['ingredients']:
        total_schedule += ing_tree['schedule']

    tree['total_schedule'] = total_schedule

    return tree


def print_dependency_tree(tree, indent_level=0):
    def is_interesting(v):
        if isinstance(v, Schedule):
            return len(v) > 0
        else:
            return v > 0

    req_strings = ['{}: {}'.format(k, v) for k, v in tree['schedule'].items() if is_interesting(v)]

    print('{}{} ({})'.format(' ' * indent_level, tree['name'], ', '.join(req_strings)))

    for ing_tree in tree['ingredients']:
        print_dependency_tree(ing_tree, indent_level + 2)

science_pack_rate = config['lab-count'] / additional_data['lab-research-time']

for display_name, sci_recipe_name in science_types:
    print('=== {} ===\n'.format(display_name))
    print_dependency_tree(build_dependency_tree(sci_recipe_name, science_pack_rate))
    print()
