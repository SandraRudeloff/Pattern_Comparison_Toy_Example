import os
import random

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

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

    total_flow.index = total_flow.index.astype("int32")
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


def visualize_flow_for_chain(
    investigation_scenario, selected_chain_name, df_shops, total_flow
):
    if df_shops.index.name != "cell_id":
        df_shops.set_index("cell_id", inplace=True)

    total_flow.index = total_flow.index.astype("int64")

    filtered_shops = get_stores(selected_chain_name, df_shops)
    sales_per_cell = get_production_potential(df_shops)

    filtered_flow = get_flow_for_chain(sales_per_cell, filtered_shops, total_flow)

    # Calculate the cumulative inflow to each cell
    cumulative_inflow = filtered_flow.sum()

    n = int(np.sqrt(len(cumulative_inflow)))  # Assuming the grid is square
    cumulative_inflow_matrix = np.array(cumulative_inflow).reshape((n, n))

    # Plot the heatmap
    fig, ax = plt.subplots(figsize=(10, 10))
    ax.set_aspect("equal", "box")
    custom_cubehelix = sns.cubehelix_palette(
        gamma=0.5, start=2, rot=0, as_cmap=True, dark=0.3
    )

    sns.heatmap(
        cumulative_inflow_matrix,
        cmap=custom_cubehelix,
        cbar=False,
        annot=True,
        annot_kws={"size": 5},  # Set annotation size
        fmt=".1f",  # Control number of decimal places
        linewidths=0.2,
        linecolor="gray",
        ax=ax,
    )

    ax.set_title(f"Flows from stores of {selected_chain_name}")

    # Reverse the y-axis to place (0,0) at the lower-left corner
    ax.invert_yaxis()

    ax.set_xticks(np.arange(0, n + 1, n / 5))
    ax.set_yticks(np.arange(0, n + 1, n / 5))
    ax.set_xticklabels(np.round(np.linspace(0, 1, 6), 1))
    ax.set_yticklabels(np.round(np.linspace(0, 1, 6), 1))
    ax.tick_params(axis="both", which="major", labelsize=6, colors="darkgray")

    max_value = np.round(np.max(cumulative_inflow_matrix), 1)
    intermediate_ticks = [np.round(max_value * x, 1) for x in [0.25, 0.5, 0.75]]

    cbar = plt.colorbar(ax.collections[0], ax=ax)
    cbar.set_ticks([0] + intermediate_ticks + [max_value])
    cbar.set_label("Inflow Value")

    # Overlay the store locations
    for _, row in filtered_shops.iterrows():
        plt.scatter(row["x"] * n, row["y"] * n, color="black", s=35, marker="D")
    investigation_scenario = int(investigation_scenario)

    # Save the plot
    directory_path = (
        f"../Traceback_Model/Data/Results/Scenario_{investigation_scenario}"
    )
    if not os.path.exists(directory_path):
        os.makedirs(directory_path)

    plot_filename = os.path.join(
        directory_path,
        f"Scenario_{investigation_scenario}_Flows_from_stores_of_{selected_chain_name}.png",
    )
    fig.savefig(plot_filename)
    plt.close(fig)

    return fig, ax
