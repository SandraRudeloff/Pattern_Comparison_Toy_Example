import pandas as pd

from gravity_model import hyman_model
from outbreak_generation import *

# Create the data
population_data = {
    "x_centroid": [i * 0.1 + 0.05 for i in range(10) for _ in range(10)],
    "y_centroid": [i * 0.1 + 0.05 for _ in range(10) for i in range(10)],
    "population": [5.0] * 100,
    "cell_id": [float(i + 1) for i in range(100)],
}

# Create the DataFrame
df_population = pd.DataFrame(population_data)

# Display the DataFrame
print(df_population)

# Create the data
shops_data = {
    "x": [0.05, 0.85, 0.05, 0.35, 0.65, 0.15],
    "y": [0.35, 0.75, 0.55, 0.15, 0.65, 0.45],
    "sales": [333.0] * 6,
    "chain": ["Chain 1", "Chain 1", "Chain 1", "Chain 2", "Chain 2", "Chain 2"],
    "cell_id": [31, 79, 51, 14, 67, 42],
}

# Create the DataFrame
df_shops = pd.DataFrame(shops_data)

# Display the DataFrame
print(df_shops)

if df_shops.index.name != "cell_id":
    df_shops.set_index("cell_id", inplace=True)

if df_population.index.name != "cell_id":
    df_population.set_index("cell_id", inplace=True)

total_flow = hyman_model(0.2, 0.001, df_population, df_shops)
total_flow.index = total_flow.index.astype(int)


selected_stores = get_stores("Chain 1", df_shops)
sales_per_cell = get_production_potential(df_shops)


flow_Chain_1 = get_flow_for_chain(sales_per_cell, selected_stores, total_flow)
