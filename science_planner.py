import yaml
import math
from itertools import count
from operator import itemgetter

with open('data/configuration.yaml') as yconfig:
    config = yaml.safe_load(yconfig)

with open('data/recipes.15.31.yaml') as yrecipes:
    recipes = yaml.safe_load(yrecipes)

with open('data/additional-data.yaml') as ydata:
    additional_data = yaml.safe_load(ydata)


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


def is_chemistry_recipe(recipe_name):
    return is_recipe_of_category(recipe_name, 'chemistry')


def crack_light(light_rate):
    rec = recipes['light-oil-cracking']
    frac = float(rec['result_count'])/rec['ingredients']['light-oil']
    return light_rate * frac


def crack_heavy_petrol(heavy_rate):
    rec = recipes['heavy-oil-cracking']
    frac = float(rec['result_count'])/rec['ingredients']['heavy-oil']
    return crack_light(heavy_rate * frac)


def crack_heavy_light(heavy_rate):
    rec = recipes['heavy-oil-cracking']
    frac = float(rec['result_count'])/rec['ingredients']['heavy-oil']
    return heavy_rate * frac


# todo: cracking
def calculate_oil(dependency_tree):

    chemistry_schedule = Schedule({ing['name']: ing['desired rate']
                                   for ing in dependency_tree['ingredients']
                                   if is_chemistry_recipe(ing['name'])})

    chemistry_schedule.add('lubricant', 0.0)  # avoid key error next line
    # lubricant_rate = chemistry_schedule['lubricant']

    oil_products = Schedule()
    for chem_recipe, rate in chemistry_schedule.items():
        for ingredient, ing_count in ingredients(chem_recipe).items():
            if ingredient in ['petroleum-gas', 'light-oil', 'heavy-oil']:
                oil_products.add(ingredient, ing_count * rate)
    if config['advanced-oil-processing']:
        oil_recipe = recipes['basic-oil-processing']
        # cracking_enabled = True
    else:
        oil_recipe = recipes['basic-oil-processing']
        # cracking_enabled = False

    fraction_rates = dict(oil_products.items())
    crude_rates = dict()

    def crude_frac(f):
        return float(oil_recipe['results'][f])/oil_recipe['ingredients']['crude-oil']

    for fraction in oil_recipe['results']:
        if fraction not in fraction_rates:
            crude_rates['fraction'] = 0.0
        else:
            crude_rates[fraction] = fraction_rates[fraction] / crude_frac(fraction)

    limiting_frac, crude_rate = max(fraction_rates.items(), key=itemgetter(1))

    if crude_rate > 0:
        dependency_tree['schedule'].add('oil-yield', crude_rate * 10)

    for ing_tree in dependency_tree['ingredients']:
        calculate_oil(ing_tree)


"""
    if cracking_enabled:
        crackable_heavy_rate = fraction_rates['heavy-oil'] - lubricant_rate
        product_rates = {f: crude_rate * crude_frac(f) for f in fraction_rates}

    if cracking_enabled and limiting_frac == 'light-oil':
        crude_rate -= crack_heavy_light(crackable_heavy_rate)
"""


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
    tree['desired rate'] = desired_rate
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

    calculate_oil(tree)

    tree['total_schedule'] = total_schedule

    return tree

production_cycle_length = config['production-cycle-length']


def print_dependency_schedule(name, schedule, rate, indent_level=0):
    def is_interesting(v):
        if isinstance(v, Schedule):
            return len(v) > 0
        else:
            return v > 0

    req_strings = ['{}: {}'.format(k, v) for k, v in schedule.items() if is_interesting(v)]

    print('{}{:>25s}: {:.2e}/cycle ({})'
          .format(' ' * indent_level, name, rate * production_cycle_length, ', '.join(req_strings)))


def print_dependency_tree(tree, indent_level=0):
    print_dependency_schedule(tree['name'], tree['schedule'], tree['desired rate'], indent_level)
    for ing_tree in tree['ingredients']:
        print_dependency_tree(ing_tree, indent_level + 25)


def recipe_totals(tree):
    total_recipe_schedules = Schedule()
    total_recipe_rates = Schedule()
    total_recipe_schedules.add(tree['name'], tree['schedule'])
    total_recipe_rates.add(tree['name'], tree['desired rate'])

    for ing_tree in tree['ingredients']:
        ing_totals, ing_rates = recipe_totals(ing_tree)
        total_recipe_schedules += ing_totals
        total_recipe_rates += ing_rates

    return total_recipe_schedules, total_recipe_rates

science_pack_rate = config['lab-count'] / additional_data['lab-research-time']

for idx, (display_name, sci_recipe_name) in zip(count(0, 1), science_types):
    print('=== {} ==='.format(display_name))

    if idx > 2:  # advanced oil processing after blue science
        config['advanced-oil-processing'] = True

    dep_tree = build_dependency_tree(sci_recipe_name, science_pack_rate)
    # print_dependency_tree(dep_tree)

    print()

    totals, rates = recipe_totals(dep_tree)
    sorted_deps = sorted(dict(totals.items()), key=lambda rn: (rates[rn], -1 * totals[rn]['assemblers']))
    for dep_rn in sorted_deps:
        print_dependency_schedule(dep_rn, totals[dep_rn], rates[dep_rn])

    print()
