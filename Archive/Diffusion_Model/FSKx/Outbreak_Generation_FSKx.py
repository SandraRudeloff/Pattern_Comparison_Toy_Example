import os
import random

import pandas as pd

subfolder = "FSKx"
##### Definition of input Data #####
## Outbreak sizes ##
list_outbreak_scenario_sizes = [10]

no_of_trials_per_scenario = 1


def get_production_potential(shops_data):
    production_potential = shops_data.groupby("Gitter_ID").agg(
        Markets_Count=("ID", "count"),
        production_potential=("Sales", "sum"),
    )
    return production_potential


def get_flow(sales_per_cell, selected_stores):
    # First we need to get all cells in which there are two stores:
    flow = pd.read_pickle(os.path.join(subfolder, "Outputs", "Flow", "flow.pkl"))

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

    selected_flow = pd.concat([selected_flow, adjusted_rows])
    return selected_flow


def get_stores(chain_name, all_stores):
    selected_stores = all_stores[all_stores["Chain"] == chain_name]
    return selected_stores


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


def generate_outbreak(chain_name, no_of_cases, all_stores):
    selected_stores = get_stores(chain_name, all_stores)

    sales_per_cell = get_production_potential(all_stores)

    flow = get_flow(sales_per_cell, selected_stores)

    cumulative_distribution = get_cumulative_distribution(flow)

    outbreak_scenario = []

    outbreak_scenario = [
        get_location_for_outbreak(cumulative_distribution) for _ in range(no_of_cases)
    ]

    return outbreak_scenario


def get_xy(outbreak_scenario):
    df = pd.DataFrame({"Gitter_ID": outbreak_scenario})
    population_data = pd.read_pickle(
        os.path.join(subfolder, "Outputs", "Population", "population.pkl")
    )
    df = df.merge(
        population_data[["x_centroid", "y_centroid"]],
        on="Gitter_ID",
        how="left",
    )
    return df


# As we want to make the artificial Outbreaks reproducible, we set the seed for the generation of random numbers
# random.seed(3)

all_stores = pd.read_pickle(os.path.join(subfolder, "Outputs", "Stores", "stores.pkl"))
no_of_chains = all_stores["Chain"].nunique()
# Number of stores per chain
chains = all_stores.groupby(["Chain"])["Chain"].agg("count")

for chain in chains.index:
    for no_of_outbreak_cases in list_outbreak_scenario_sizes:
        for trial in range(0, no_of_trials_per_scenario):
            outbreak_name = f"{chain}_{no_of_outbreak_cases}_{trial}"

            outbreak_scenario_cells = generate_outbreak(
                chain, no_of_outbreak_cases, all_stores
            )

            outbreak_scenario = get_xy(outbreak_scenario_cells)

            os.makedirs(os.path.join(subfolder, "Outputs", "Outbreaks"), exist_ok=True)
            outbreak_scenario.to_pickle(
                os.path.join(
                    subfolder, "Outputs", "Outbreaks", f"Outbreak_{outbreak_name}.pkl"
                )
            )
