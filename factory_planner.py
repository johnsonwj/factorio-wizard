import yaml
import sys
import math

with open('data/configuration.yaml') as yconfig:
    config = yaml.safe_load(yconfig)

with open('data/recipes.15.31.yaml') as yrecipes:
    recipes = yaml.safe_load(yrecipes)


def unalias(alias_name):
    aliases = config['aliases']
    if alias_name in aliases:
        return aliases[alias_name]
    return alias_name


def get_science_recipe(science_color):
    return recipes[unalias('{}-science'.format(science_color))]

sciences = map(get_science_recipe, config['science'])


def make_outposts(outpost_name, outpost_specs):
    stockpile_period = 60.0 * config['stockpile-period']
    schedules = [{recipe_name: stockpile_size / stockpile_period
                  for (recipe_name, stockpile_size) in spec.items()}
                 for spec in outpost_specs]

    return [(outpost_name, idx, schedule) for (idx, schedule) in zip(range(len(schedules)), schedules)]

outposts = [ophase for item in config['outposts'].items() for ophase in make_outposts(*item)]

factory = dict()
factory_upgrades = list()


def can_smelt():
    return 'coal' in factory


def ingredients(recipe_name):
    actual_recipe_name = unalias(recipe_name)
    if actual_recipe_name in recipes:
        return recipes[actual_recipe_name]['ingredients']
    return dict()


def available_onsite_recipes():
    onsite_requirements = {rn: ingredients(rn) for rn in config['onsite']}
    factory_recipes = [unalias(rn) for op in factory.values() for rn in op[1]]
    return [os for os in onsite_requirements if all(i in factory_recipes for i in onsite_requirements[os])]


def is_recipe_of_category(recipe_name, category):
    actual_recipe_name = unalias(recipe_name)

    if actual_recipe_name not in recipes:
        return False

    recipe = recipes[actual_recipe_name]
    if 'category' in recipe:
        return recipe['category'] == category
    return False


def is_smelting_recipe(recipe_name):
    return is_recipe_of_category(recipe_name, 'smelting')


def can_produce(recipe_name):
    recipe_ingredients = ingredients(recipe_name)
    onsite_recipes = available_onsite_recipes()
    factory_recipes = [rn for on, (p, s) in factory.items() for rn in s]

    all_available_recipes = [unalias(a) for a in factory_recipes + onsite_recipes]

    # something that is mined directly
    if len(recipe_ingredients) == 0:
        return True
    elif can_smelt() and is_smelting_recipe(recipe_name):
        return True

    return all([i in all_available_recipes for i in recipe_ingredients])


def satisfied_dependencies(outpost):
    outpost_recipes = [rn for rn in outpost[2]]
    return all([can_produce(rn) for rn in outpost_recipes])


def split(predicate, xs):
    p_matches = [l for l in xs if predicate(l)]
    p_misses = [l for l in xs if not predicate(l)]

    return p_matches, p_misses


def add_or_insert(d, k, v):
    if k in d:
        d[k] += v
    else:
        d[k] = v


def sum_dict_values(s1, s2):
    total = dict()

    for k, v in list(s1.items()) + list(s2.items()):
        add_or_insert(total, k, v)

    return total


def crafting_effort(recipe_name):
    actual_recipe_name = unalias(recipe_name)
    if actual_recipe_name not in recipes:
        return 0.0

    recipe = recipes[actual_recipe_name]

    if 'result_count' in recipe:
        result_count = recipe['result_count']
    else:
        result_count = 1

    if 'energy_required' in recipe:
        return recipe['energy_required'] / result_count

    return 0.5 / result_count # default


def total_rates(recipe_name, recipe_rate):
    mining_speed = config['additional-data']['mining-speed']

    total_mining_rates = dict()
    total_smelting_rates = dict()
    total_crafting_rates = dict()

    if recipe_name in mining_speed:
        add_or_insert(total_mining_rates, recipe_name, recipe_rate)
    elif is_smelting_recipe(recipe_name):
        add_or_insert(total_smelting_rates, recipe_name, recipe_rate)
    else:
        add_or_insert(total_crafting_rates, recipe_name, recipe_rate)

        ingredient_schedule = {ing_name: ing_count * recipe_rate
                               for ing_name, ing_count in ingredients(recipe_name).items()}

        ing_mining_rates, ing_smelting_rates, ing_crafting_rates = schedule_total_rates(ingredient_schedule)

        total_mining_rates = sum_dict_values(total_mining_rates, ing_mining_rates)
        total_smelting_rates = sum_dict_values(total_smelting_rates, ing_smelting_rates)
        total_crafting_rates = sum_dict_values(total_crafting_rates, ing_crafting_rates)

    return total_mining_rates, total_smelting_rates, total_crafting_rates


def schedule_total_rates(schedule):
    total_mining_rates = dict()
    total_smelting_rates = dict()
    total_crafting_rates = dict()

    for recipe_name, recipe_rate in schedule.items():
        ing_mining_rates, ing_smelting_rates, ing_crafting_rates = total_rates(recipe_name, recipe_rate)

        total_mining_rates = sum_dict_values(total_mining_rates, ing_mining_rates)
        total_smelting_rates = sum_dict_values(total_smelting_rates, ing_smelting_rates)
        total_crafting_rates = sum_dict_values(total_crafting_rates, ing_crafting_rates)

    return total_mining_rates, total_smelting_rates, total_crafting_rates


def total_schedule(outpost_schedules):
    totals = dict()

    for oname, (new_phase, new_schedule) in outpost_schedules.items():
        totals = sum_dict_values(totals, new_schedule)

    return totals


def upgrade_requirements(upgrade):
    ts = total_schedule(upgrade)

    mining_speed = config['additional-data']['mining-speed']
    smelting_speed = config['additional-data']['smelting-speed']
    crafting_speed = config['additional-data']['crafting-speed']

    total_mining_rates, total_smelting_rates, total_crafting_rates = schedule_total_rates(ts)

    def ru(f):
        return int(math.ceil(f))

    smelters_needed = {recipe_name: ru(rate / smelting_speed['steel-furnace'])
                       for recipe_name, rate in total_smelting_rates.items()}

    miners_needed = {recipe_name: ru(rate / mining_speed[recipe_name])
                     for recipe_name, rate in total_mining_rates.items()}

    crafters_needed = {recipe_name: ru(rate * crafting_effort(recipe_name) / crafting_speed['assembling-machine-2'])
                       for recipe_name, rate in total_crafting_rates.items()}

    return miners_needed, smelters_needed, crafters_needed


def add_to_factory(upgrade):
    miners, smelters, crafters = upgrade_requirements(upgrade)

    upgrade_spec = {
        'new_production': {oname: list(schedule.keys()) for oname, (phase, schedule) in upgrade.items()},
        'miners': miners,
        'smelters': smelters,
        'crafters': crafters
    }

    factory_upgrades.append(upgrade_spec)

    for oname, (new_phase, new_schedule) in upgrade.items():
        if oname in factory:
            old_phase, old_schedule = factory[oname]
            factory[oname] = (new_phase, sum_dict_values(new_schedule, old_schedule))
        else:
            factory[oname] = (new_phase, new_schedule)


def print_results():
    miners, smelters, crafters = upgrade_requirements(factory)

    for oname, (phase, sched) in factory.items():
        print('{}-{:d}: {}'.format(oname, phase, list(sched.keys())))

    for caption, reqs in [('miners', miners), ('smelters', smelters), ('crafters', crafters)]:
        print('{}:'.format(caption))
        for req_name, count in reqs.items():
            print('  {}: {:d}'.format(req_name, count))
        print()


while len(outposts) > 0:
    available, navailable = split(satisfied_dependencies, outposts)

    if len(available) == 0:
        print("can't do anything! {}\n".format(navailable))
        print_results()
        sys.exit(1)

    f_upgrade = {oname: (phase, schedule) for oname, phase, schedule in available}
    add_to_factory(f_upgrade)

    outposts = navailable

print_results()


