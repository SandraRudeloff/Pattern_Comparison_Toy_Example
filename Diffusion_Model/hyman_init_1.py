import os
from math import sqrt

import pandas as pd

from gravity_model import hyman_model

##### Definition of input Data #####
scenario = 1
no_of_cells = 100  # should be a number that gives a square (10x10)

## Data on Shopping behavior ##
# shopping distance: 0.4 km
empirical_mean_shopping_distance = 0.4  # all units are in km
tolerance = 0.001

## Population Data ##
# uniform population of 5 in each cell (500 total)
population_per_cell = 5

## Shops Data ##
# only one chain with 5 stores distributed randomly
# The coordinates go from 0 to 1000
# The shops shouldn't be on a round number cause otherwise they're within 4 cells at the same time
# (all lists need to be the same length)
x_coord = [112, 823, 888, 105, 487]
y_coord = [198, 112, 846, 855, 537]
Chain = ["Chain 1", "Chain 1", "Chain 1", "Chain 1", "Chain 1"]
Sales = [1000, 1000, 1000, 1000, 1000]


def check_input_data():
    # check whether all lists have the same length
    lists = [x_coord, y_coord, Chain, Sales]
    it = iter(lists)
    the_len = len(next(it))
    if not all(len(l) == the_len for l in it):
        raise ValueError(
            "Not all lists that define the shops data have the same length"
        )

    # check whether the no_of_cells gives a perfect square
    sq_root = int(sqrt(no_of_cells))
    if (sq_root * sq_root) != no_of_cells:
        raise ValueError("Number of cells doesn't give a perfect square")


def import_population_data(no_of_cells, population_per_cell):
    df_population = pd.DataFrame(columns=["population", "x_centroid", "y_centroid"])
    # set values
    y = -50
    x = 50
    for i in range(0, no_of_cells):
        if (i / 10).is_integer():
            y = y + 100
            x = 50
        df_population.loc[i] = pd.Series(
            {
                "y_centroid": y,
                "x_centroid": x,
            }
        )
        x = x + 100

    df_population.index.names = ["Gitter_ID"]

    df_population["population"] = population_per_cell

    os.makedirs("Outputs/Population", exist_ok=True)
    df_population.to_pickle("Outputs/Population/population_" + str(scenario) + ".pkl")

    return df_population


def import_shop_data(df_population):
    ID = list(range(1, len(x_coord) + 1))
    df_shops = pd.DataFrame(
        {
            "ID": ID,
            "x_coord": x_coord,
            "y_coord": y_coord,
            "Chain": Chain,
            "Sales": Sales,
            "Gitter_ID": "",
        }
    )
    for ind in df_shops.index:
        df_shops["Gitter_ID"][ind] = (
            df_population[
                ((df_population["x_centroid"] - 50) <= df_shops.x_coord[ind])
                & ((df_population["x_centroid"] + 50) >= df_shops.x_coord[ind])
                & ((df_population["y_centroid"] - 50) <= df_shops.y_coord[ind])
                & ((df_population["y_centroid"] + 50) >= df_shops.y_coord[ind])
            ].index.values
        )[0]

    os.makedirs("Outputs/Stores", exist_ok=True)
    df_shops.to_pickle("Outputs/Stores/stores_" + str(scenario) + ".pkl")

    return df_shops


check_input_data()
df_population = import_population_data(no_of_cells, population_per_cell)

# Shops Data
df_shops = import_shop_data(df_population)

flow = hyman_model(empirical_mean_shopping_distance, tolerance, df_population, df_shops)

os.makedirs("Outputs/Flow", exist_ok=True)
flow.to_pickle("Outputs/Flow/flow_" + str(scenario) + ".pkl")
