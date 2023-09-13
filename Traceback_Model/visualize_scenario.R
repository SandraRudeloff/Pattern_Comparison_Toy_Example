visualize_scenario <- function(investigation_scenario, df_shops, df_population, df_outbreak){
  # Assign colors to different chains ---- 
  
  # Identify unique chains and generate a color palette
  unique_chains <- unique(df_shops$chain)
  
  # Generate a color palette avoiding red and blue
  all_colors <- brewer.pal(9, "Set1")  # This palette has distinct colors
  all_colors <- all_colors[!all_colors %in% c("#E41A1C", "#377EB8")] 
  chain_colors <- all_colors[1:length(unique_chains)]
  
  names(chain_colors) <- unique_chains
  
  # Create the main plot with population, stores, and outbreak cases
  p_main <- ggplot() +
    # Plot the population data
    geom_tile(
      data = df_population,
      aes(x = x_centroid, y = y_centroid, fill = population),
      width = 0.1,
      height = 0.1,
      alpha = 0.8
    ) +
    scale_fill_gradient(
      low = "white",
      high = "cadetblue",
      guide = "none"
    ) +
    
    # Introduce a new scale for the shop chains
    new_scale_fill() +
    
    # Plot the shops data
    geom_point(
      data = df_shops,
      aes(x = store_x, y = store_y, fill = chain),
      size = 3,
      shape = 23,
      alpha = 0.8
    ) +
    scale_fill_manual(values = chain_colors, name = "Shop Chain") +
    
    # Plot the outbreak data
    geom_point(
      data = df_outbreak,
      aes(x = x_centroid , y = y_centroid),
      color = "red",
      size = 2,
      shape = 21,
      fill = "red",
      alpha = 0.8
    ) +
    
    # Adjust the x and y axis breaks to have lines every 100m
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
    
    # Add labels and theme
    labs(
      title = sprintf("Visualization of Scenario %s", investigation_scenario),
      x = "X Coordinate",
      y = "Y Coordinate",
      color = "Shop Chain"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      aspect.ratio = 1,
      panel.grid.minor = element_blank()
    )
  
  # Create a custom legend for the population
  legend_df <- data.frame(
    population = seq(min(df_population$population), max(df_population$population), length.out = 100)
  )
  p_legend <- ggplot(legend_df, aes(x = 1, y = population, fill = population)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "cadetblue") +
    scale_y_continuous(breaks = floor(seq(min(df_population$population), max(df_population$population), length.out = 5))) +  # Add labels to the legend
    theme_minimal() +
    labs(title = "", x = "", y = "") + 
    theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank(),  # Remove x-axis
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # Remove grid lines
  
  # Combine the main plot and the custom legend
  p_combined <- grid.arrange(p_main, p_legend, ncol = 2, widths = c(4, 1))
  return(p_combined)
}