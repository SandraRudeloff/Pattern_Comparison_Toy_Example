import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from shapely.geometry import Polygon

scenario = 1
# TODO:  Filter Visualization for one Chain
chain_name = "Chain 1"
df_outbreak = pd.read_pickle(
    "Outputs/Outbreaks/Scenario_" + str(scenario) + "/Outbreak_Chain 1_10_0.pkl"
)

df_stores = pd.read_pickle("Outputs/Stores/stores_" + str(scenario) + ".pkl")
gdf_stores = gpd.GeoDataFrame(
    df_stores,
    geometry=gpd.points_from_xy(df_stores["x_coord"], df_stores["y_coord"]),
)


# gdf_outbreak = gpd.GeoDataFrame(
#     df_outbreak,
#     geometry=gpd.points_from_xy(df_outbreak["x_centroid"], df_outbreak["y_centroid"]),
# )
gdf_outbreak = gpd.GeoDataFrame(
    df_outbreak,
    geometry=gpd.points_from_xy(
        [750, 150, 50, 450, 50, 850, 850, 950, 50, 750],
        [150, 250, 250, 450, 850, 150, 850, 150, 150, 850],
    ),
)
df_population = pd.read_pickle(
    "Outputs/Population/population_" + str(scenario) + ".pkl"
)
# Compute the latitude and longitude values for each Polygon given the centroid
ls_polygon_geometries = []
for i in range(len(df_population)):
    lon_point_list = [
        df_population.iloc[i, 1] - 50,
        df_population.iloc[i, 1] - 50,
        df_population.iloc[i, 1] + 50,
        df_population.iloc[i, 1] + 50,
    ]
    lat_point_list = [
        df_population.iloc[i, 2] - 50,
        df_population.iloc[i, 2] + 50,
        df_population.iloc[i, 2] + 50,
        df_population.iloc[i, 2] - 50,
    ]
    polygon_geom = Polygon(zip(lon_point_list, lat_point_list))
    ls_polygon_geometries.append(polygon_geom)
gdf_population = gpd.GeoDataFrame(df_population, geometry=ls_polygon_geometries)

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

# gdf_census.boundary.plot() # to show the boundaries
plt.show()
