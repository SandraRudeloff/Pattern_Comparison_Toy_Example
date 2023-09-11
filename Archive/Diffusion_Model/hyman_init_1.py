import math
import os

import numpy as np
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
    if not all(len(l) == len(lists[0]) for l in lists):
        raise ValueError(
            "Not all lists that define the shops data have the same length"
        )

    # check whether the no_of_cells gives a perfect square
    if math.isqrt(no_of_cells) ** 2 != no_of_cells:
        raise ValueError("Number of cells doesn't give a perfect square")


def import_population_data(no_of_cells, population_per_cell):
    # set values
    sqrt_cells = int(math.sqrt(no_of_cells))
    y_values = np.repeat(np.arange(50, 1000, 100), sqrt_cells)
    x_values = np.tile(np.arange(50, 1000, 100), sqrt_cells)
    df_population = pd.DataFrame(
        {
            "population": population_per_cell,
            "x_centroid": x_values,
            "y_centroid": y_values,
        }
    )
    df_population.index.names = ["Gitter_ID"]

    output_dir = os.path.join("Outputs", "Population")
    os.makedirs(output_dir, exist_ok=True)

    df_population.to_pickle(os.path.join(output_dir, f"population_{scenario}.pkl"))

    return df_population


def import_shop_data(df_population):
    df_shops = pd.DataFrame(
        {
            "ID": range(1, len(x_coord) + 1),
            "x_coord": x_coord,
            "y_coord": y_coord,
            "Chain": Chain,
            "Sales": Sales,
            "Gitter_ID": "",
        }
    )
    for ind in df_shops.index:
        df_shops.loc[ind, "Gitter_ID"] = (
            df_population[
                ((df_population["x_centroid"] - 50) <= df_shops.x_coord[ind])
                & ((df_population["x_centroid"] + 50) >= df_shops.x_coord[ind])
                & ((df_population["y_centroid"] - 50) <= df_shops.y_coord[ind])
                & ((df_population["y_centroid"] + 50) >= df_shops.y_coord[ind])
            ].index.values
        )[0]

    output_dir = os.path.join("Outputs", "Stores")
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    df_shops.to_pickle(os.path.join(output_dir, f"stores_{scenario}.pkl"))

    return df_shops


check_input_data()
df_population = import_population_data(no_of_cells, population_per_cell)

# Shops Data
df_shops = import_shop_data(df_population)

flow = hyman_model(empirical_mean_shopping_distance, tolerance, df_population, df_shops)

os.makedirs("Outputs/Flow", exist_ok=True)
flow.to_pickle("Outputs/Flow/flow_" + str(scenario) + ".pkl")
