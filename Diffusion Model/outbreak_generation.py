import random

import pandas as pd

from gravity_model import get_production_potential

# As we want to make the artificial Outbreaks reproducible, we set the seed for the generation of random numbers
random.seed(123)


def get_stores(chain_name, all_stores):
    return all_stores[all_stores["chain"] == chain_name]


def get_flow_for_chain(sales_per_cell, selected_stores, total_flow):
    # select all flows from cells where there is a store of the given chain inside
    selected_flow = total_flow[total_flow.index.isin(selected_stores.index)]

    # These flows are correct unless there is more than the one store of the given chain in any cell
    # First we only selected the cells in which there are more than one store
    multi_store_cells = sales_per_cell[sales_per_cell["stores_count"] > 1]

    selected_flow = selected_flow.merge(
        multi_store_cells["production_potential"],
        left_index=True,
        right_index=True,
        how="left",
    )

    selected_flow = selected_flow.merge(
        selected_stores["sales"],
        left_index=True,
        right_index=True,
        how="left",
    )

    adjusted_rows = (
        selected_flow.loc[selected_flow["production_potential"].notna()]
        .iloc[:, 0:-2]
        .multiply(
            (
                selected_flow.loc[selected_flow["production_potential"].notna()].sales
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

    selected_flow = pd.concat([selected_flow, adjusted_rows])

    return selected_flow


def get_cumulative_distribution(flow):
    total_sales = flow.values.sum()

    flow = flow.T

    flow["ingoing_sum"] = flow.sum(axis=1)
    flow["percent"] = flow["ingoing_sum"] / total_sales
    flow["cumulated"] = flow["percent"].cumsum()

    flow = flow.iloc[:, -3:]
    return flow[["ingoing_sum", "percent", "cumulated"]]


def get_location_for_outbreak(cumulative_distribution):
    random_number = random.random()
    return cumulative_distribution[
        cumulative_distribution["cumulated"] > random_number
    ].index[0]


def create_outbreak_scenario(chain_name, no_of_cases, all_stores, total_flow):
    selected_stores = get_stores(chain_name, all_stores)
    sales_per_cell = get_production_potential(all_stores)

    flow = get_flow_for_chain(sales_per_cell, selected_stores, total_flow)

    cumulative_distribution = get_cumulative_distribution(flow)

    outbreak_scenario = []

    outbreak_scenario = [
        get_location_for_outbreak(cumulative_distribution) for _ in range(no_of_cases)
    ]

    return outbreak_scenario


def get_xy(outbreak_scenario, population_data):
    df = pd.DataFrame({"cell_id": outbreak_scenario})
    df["cell_id"] = df["cell_id"].astype("int64")

    df = df.merge(
        population_data[["x_centroid", "y_centroid"]],
        left_on="cell_id",
        right_index=True,
        how="left",
    )

    return df


def generate_outbreak(
    chain,
    no_of_outbreak_cases,
    total_flow,
    shops_data,
    population_data,
):
    if shops_data.index.name != "cell_id":
        shops_data.set_index("cell_id", inplace=True)

    total_flow.index = total_flow.index.astype(int)

    if population_data.index.name != "cell_id":
        population_data.set_index("cell_id", inplace=True)

    population_data.index = population_data.index.astype(int)

    outbreak_scenario_cells = create_outbreak_scenario(
        chain, no_of_outbreak_cases, shops_data, total_flow
    )

    outbreak_scenario = get_xy(outbreak_scenario_cells, population_data)

    return outbreak_scenario
