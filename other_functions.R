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



