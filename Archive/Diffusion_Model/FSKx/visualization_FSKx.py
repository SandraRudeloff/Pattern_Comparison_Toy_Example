import os

import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from shapely.geometry import Polygon

subfolder = "FSKx"
chain_name = "Chain 1"
df_outbreak = pd.read_pickle(
    os.path.join(subfolder, "Outputs", "Outbreaks", "Outbreak_Chain 1_10_0.pkl")
)
df_stores = pd.read_pickle(os.path.join(subfolder, "Outputs", "Stores", "stores.pkl"))
df_population = pd.read_pickle(
    os.path.join(subfolder, "Outputs", "Population", "population.pkl")
)

# Create Points for stores and outbreak
df_stores["geometry"] = gpd.points_from_xy(df_stores["x_coord"], df_stores["y_coord"])
df_outbreak["geometry"] = gpd.points_from_xy(
    df_outbreak["x_centroid"], df_outbreak["y_centroid"]
)

# Create Polygons for population
df_population["geometry"] = df_population.apply(
    lambda row: Polygon(
        [
            (row["x_centroid"] - 50, row["y_centroid"] - 50),
            (row["x_centroid"] - 50, row["y_centroid"] + 50),
            (row["x_centroid"] + 50, row["y_centroid"] + 50),
            (row["x_centroid"] + 50, row["y_centroid"] - 50),
        ]
    ),
    axis=1,
)

# Convert to GeoDataFrames
gdf_stores = gpd.GeoDataFrame(df_stores)
gdf_outbreak = gpd.GeoDataFrame(df_outbreak)
gdf_population = gpd.GeoDataFrame(df_population)

fig, ax = plt.subplots(1, 1)

gdf_population.plot(
    ax=ax,
    column="population",
    # scheme="equal_interval",
    # k=15,
    # legend=True,
    # legend_kwds=dict(
    #     loc="upper right",
    #     bbox_to_anchor=(1.4, 1),
    #     fontsize="small",
    #     title="Legend",
    #     frameon=False,
    # ),
    edgecolor="#ebebeb",
    cmap="BuPu",
)

# TODO: When there is multiple outbreak cases in one cell, this is not visible (or in the same as a store?), maybe use a different marker for stores? also all not selected stores in different gray scales?
gdf_outbreak.plot(ax=ax, marker="o", color="red", markersize=10, zorder=2)

gdf_stores[gdf_stores["Chain"] == chain_name].plot(
    ax=ax, marker="s", color="gold", markersize=10, zorder=3
)

gdf_stores[gdf_stores["Chain"] != chain_name].plot(
    ax=ax, marker="s", color="#d3d3d3", markersize=8, zorder=3
)

plt.show()
