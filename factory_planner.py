import yaml
import sys

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
    schedules = [
        {recipe_name: stockpile_size / stockpile_period
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
    return list()


def available_onsite_recipes():
    onsite_requirements = {rn: ingredients(rn) for rn in config['onsite']}
    factory_recipes = [unalias(rn) for op in factory.values() for rn in op[1]]
    return [os for os in onsite_requirements if all(i in factory_recipes for i in onsite_requirements[os])]


def is_smelting_recipe(recipe_name):
    actual_recipe_name = unalias(recipe_name)
    recipe = recipes[actual_recipe_name]
    if 'category' in recipe:
        return recipe['category'] == 'smelting'
    return False


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


def sum_schedules(s1, s2):
    total = dict()

    for rn, rate in s1.items():
        if rn in s2:
            total[rn] = rate + s2[rn]
            del s2[rn]
        else:
            total[rn] = rate

    # all the shared ones have already been deleted
    for rn, rate in s2.items():
        total[rn] = rate

    return total


def add_to_factory(upgrade):
    factory_upgrades.append(upgrade)

    for oname, (new_phase, new_schedule) in upgrade.items():
        if oname in factory:
            old_phase, old_schedule = factory[oname]
            factory[oname] = (new_phase, sum_schedules(new_schedule, old_schedule))
        else:
            factory[oname] = (new_phase, new_schedule)


def print_results():
    for ug in factory_upgrades:
        print(ug)

    print()

    print(factory)


while len(outposts) > 0:
    available, navailable = split(satisfied_dependencies, outposts)

    if len(available) == 0:
        print("can't do anything! {}".format(navailable))
        print_results()
        sys.exit(1)

    upgrade = {oname: (phase, schedule) for oname, phase, schedule in available}
    add_to_factory(upgrade)

    outposts = navailable

print_results()


