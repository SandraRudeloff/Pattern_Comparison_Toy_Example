import os

import pandas as pd

# from gravity_model import *
from monte_carlo_simulation import generate_outbreak


def get_xy(outbreak_scenario, scenario, df_population):
    df = pd.DataFrame({"Gitter_ID": outbreak_scenario})
    population_data = df_population

    df = df.merge(
        population_data[["x_centroid", "y_centroid"]],
        on="Gitter_ID",
        how="left",
    )
    return df


def generate_outbreaks(
    investigation_scenario, df_shops, df_population, list_outbreak_scenario_sizes
):
    ## Trials per outbreak scenario size ##
    # Set the number of simulations, If only 1 store, we only simulate one outbreak
    # if chains[chain] == 1:
    #     no_of_trials_per_scenario = 1
    # else:
    #     no_of_trials_per_scenario = 5 + round(
    #         (no_of_outbreak_cases * chains[chain]) / 5
    #     )

    # Ensure list_outbreak_scenario_sizes is a list
    if not isinstance(list_outbreak_scenario_sizes, list):
        list_outbreak_scenario_sizes = [list_outbreak_scenario_sizes]
    no_of_trials_per_scenario = 1

    # As we want to make the artificial Outbreaks reproducible, we set the seed for the generation of random numbers
    # random.seed(3)

    all_stores = df_shops

    n_of_chains = all_stores["chain"].nunique()
    # Number of stores per chain
    chains = all_stores.groupby(["chain"])["chain"].agg("count")
    for chain in chains.index:
        for no_of_outbreak_cases in list_outbreak_scenario_sizes:
            for trial in range(0, no_of_trials_per_scenario):
                outbreak_name = f"{chain}_{no_of_outbreak_cases}_{trial}"

                print(chain)
                print(type(chain))
                print(no_of_outbreak_cases)
                print(all_stores)
                print(type(all_stores))
                print(investigation_scenario)
                print("ENde")
                outbreak_scenario_cells = generate_outbreak(
                    str(chain), no_of_outbreak_cases, all_stores, investigation_scenario
                )

                outbreak_scenario = get_xy(
                    outbreak_scenario_cells, investigation_scenario, df_population
                )

                output_dir = os.path.join(
                    "Outputs", "Outbreaks", f"Scenario_{investigation_scenario}"
                )
                os.makedirs(
                    output_dir,
                    exist_ok=True,
                )
                outbreak_scenario.to_pickle(
                    os.path.join(output_dir, f"Outbreak_{outbreak_name}.pkl")
                )
