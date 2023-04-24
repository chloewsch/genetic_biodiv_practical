library(sf)                # spatial data wrangling
library(rnaturalearth)     # optional; for base map
library(rnaturalearthdata) # optional; for base map
library(ggplot2)           # optional for mapping

stat_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


plot_clusters <- function(data, sf_data, basemap = ca_usa, clusters){
  h <- snapclust(data, k=clusters)
  
  gdat <- data.frame(pop = pop(data),
                     clusterID = h$group)
  
  gdatclus <- aggregate(clusterID ~ pop, data = gdat, 
                     FUN = stat_mode)
  spdat <- sf_data
  spdat$cluster <- gdatclus$clusterID[match(spdat$pop, gdatclus$pop)]
  plotc <- ggplot() + 
            geom_sf(data = basemap) +
            geom_sf(data = spdat, aes(color = cluster)) +
            coord_sf(crs = 'ESRI:102009') + 
            theme_minimal()
  plot(plotc)
}



# Summarise effects from models
plot_format <- function(model, predictor, response){
  # Extract fixed effects
  fixed <- fixef(model)[predictor]
  
  # 95% confidence interval for fixed effect:
  cofint95 <- confint(model)[predictor,]
  lo_95 <- cofint95[1]
  up_95 <- cofint95[2]
  
  # Extract random slopes
  slopes <- coef(model)[[1]]
  slope_df <- data.frame(species = rownames(slopes),
                         slope = slopes[,predictor])
  
  # Output table
  plot_sum <- data.frame(species = slope_df$species,
                         variable = predictor,
                         main_effect = fixed,
                         lower_95 = lo_95,
                         upper_95 = up_95,
                         species_effect = slope_df$slope,
                         response_var = response,
                         row.names = NULL)
  
  return(plot_sum)
}