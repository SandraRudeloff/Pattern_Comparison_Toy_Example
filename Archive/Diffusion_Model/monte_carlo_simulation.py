import os
import random

import pandas as pd

from gravity_model import *


def get_flow(sales_per_cell, selected_stores, scenario):
    # First we need to get all cells in which there are two stores:
    flow = pd.read_pickle(
        os.path.join("Outputs", "Flow", "flow_" + str(scenario) + ".pkl")
    )
    # select all flows from cells where there is a store of the given chain inside
    selected_flow = flow[flow.index.isin(selected_stores.Gitter_ID)]

    # These flows are correct unless there is more than the one store of the given chain in any cell
    # First we only selected the cells in which there are more than one store
    only_multiple = sales_per_cell[sales_per_cell["Markets_Count"] > 1]

    # Now we merge it to the existing flow
    selected_flow = selected_flow.merge(
        only_multiple["production_potential"], on="Gitter_ID", how="left"
    )
    selected_stores["Gitter_ID"] = selected_stores["Gitter_ID"].astype(int)
    selected_stores.set_index("Gitter_ID", inplace=True)
    selected_flow = selected_flow.merge(
        selected_stores["Sales"], on="Gitter_ID", how="left"
    )

    adjusted_rows = (
        selected_flow.loc[selected_flow["production_potential"].notna()]
        .iloc[:, 0:-2]
        .multiply(
            (
                selected_flow.loc[selected_flow["production_potential"].notna()].Sales
                / selected_flow.loc[
                    selected_flow["production_potential"].notna()
                ].production_potential
            ),
            axis=0,
        )
    )

    selected_flow = selected_flow[selected_flow["production_potential"].isnull()].iloc[
        :, 0:-2
    ]

    selected_flow = selected_flow.append(adjusted_rows, verify_integrity=True)

    return selected_flow


def get_stores(chain_name, all_stores):
    selected_stores = all_stores[all_stores["chain"] == chain_name]
    return selected_stores


def get_cumulative_distribution(flow):
    total_sales = flow.values.sum()

    flow = flow.T

    flow["ingoing_sum"] = flow.sum(axis=1)
    flow["percent"] = flow["ingoing_sum"] / total_sales
    flow["cumulated"] = flow["percent"].cumsum()

    flow = flow.iloc[:, -3:]
    return flow


def get_location_for_outbreak(cumulative_distribution):
    random_number = random.random()
    return cumulative_distribution[
        cumulative_distribution["cumulated"] > random_number
    ].index[0]


def generate_outbreak(chain_name, no_of_cases, all_stores, scenario):
    print(chain_name)
    print(no_of_cases)
    print(all_stores)
    print(scenario)
    selected_stores = get_stores(chain_name, all_stores)

    sales_per_cell = get_production_potential(all_stores)

    flow = get_flow(sales_per_cell, selected_stores, scenario)

    cumulative_distribution = get_cumulative_distribution(flow)

    outbreak_scenario = []
    outbreak_scenario = [
        get_location_for_outbreak(cumulative_distribution) for _ in range(no_of_cases)
    ]
    return outbreak_scenario
