# Genetic diversity practical part I: Working with genetic data in R ------
# Brief intro to making maps, genind objects, visualizing population structure, and estimating diversity metrics


# Libraries -----
#library(tidyr)
#library(dplyr)

library(sf)                 # spatial data wrangling
#library(rnaturalearth)     # optional; for base map
#library(rnaturalearthdata) # optional; for base map
library(ggplot2)            # optional for mapping

library(adegenet)           # compute diversity metrics for genetic data
library(hierfstat)          # compute diversity metrics for genetic data

# Some extra custom functions:
source('other_functions.R')

# Load the data -----
# Microsatellite genotypes and locations for snowshoe hare sampled across their geographic distribution
# Data is from: Cheng et al. (2014) "Conservation implications of the evolutionary history and genetic diversity 
# hotspots of the snowshoe hare." Molecular Ecology 23(12): 2929-2942.
# paper: https://onlinelibrary.wiley.com/doi/full/10.1111/mec.12790
# data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.dh63p

microsats <- read.table('data/Cheng_Lepus_americanus.str')
coordinates <- read.csv('data/Lepus_americanus_coordinates.csv', header = TRUE)

# Overview of the data:
head(microsats)
str(microsats)

head(coordinates)
str(coordinates)


# 1. Mapping samples -----
# We can simply plot the data like this to get an idea:
plot(coordinates$lon, coordinates$lat)
text(coordinates$lon, coordinates$lat, labels = coordinates$pop)


# But, we might want to make a nicer map to understand the spatial context of where these points are.
# Feel free to run this code if you wish (will require additional packages)-- This section is optional! #

# We'll start by telling R that the coordinates are spatial locations.
# The coordinates are from GPS, and use the 'WGS84' coordinate reference system (crs), where units are decimal degrees
# The sf package uses codes for coordinate reference systems from the EPSG: https://epsg.io/
# More on coordinate systems: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf

coords_spatial <- st_as_sf(coordinates, coords = c('lon', 'lat'), crs = 4326)

head(coords_spatial)

## Q: How did the coordinate data change? What does it look like now? -----

# These data were sampled in Canada and the US. Let's get a map of these countries:
#ca_usa <- ne_countries(country = c('Canada', 'United States of America'), returnclass = 'sf')
ca_usa <- readRDS('data/basemap.RDS')

# Plot with ggplot:
ggplot() + 
  geom_sf(data = ca_usa) +
  geom_sf(data = coords_spatial) +
  theme_minimal()

# And in a nicer projection so it's less distorted:
# coord_sf() transforms the data into a new coordinate system. This is a Lambert conformal conic projection for North America
ggplot() + 
  geom_sf(data = ca_usa) +
  geom_sf(data = coords_spatial) +
  coord_sf(crs = 'ESRI:102009') + 
  theme_minimal()



# 2. Exploring a microsatellite dataset --------------
# These data are formatted in a file type (str) for a software called STRUCTURE. 
# Rows are individuals, the first column is a group ID, and the rest of the columns are alleles.

## Snowshoe hare are diploid (two copies of each chromosome), so each locus has 2 columns.
# The data can be formatted with one individual per row, and each column is every allele of every 
# locus (Locus 1 allele 1; locus 1 allele 2; locus 2 allele 1, etc.)

# OR, the data can have each individual on two rows, where one row is allele 1, and the second row 
# is allele 2, and each column is one locus.

# In this file, individuals are on one row, and the groups are sample locations.

## Q1: how many individuals are there in this file? -----
# Hint:
nrow(microsats)

## Q2: how many loci are there in this file? -----
# Hint:
ncol(microsats)

# Now we want R to read the file knowing that it's genetic data.
# We use the function read.structure() for this:
snow <- read.structure('data/Cheng_Lepus_americanus.str') 

# What happened? Did it work?

# We need to provide more info about the samples before loading the data.

# Read the data in:
snow <- read.structure('data/Cheng_Lepus_americanus.str', # file
                               n.ind = nrow(microsats),           # number of individuals
                               n.loc = (ncol(microsats)-1)/2,     # number of loci
                               onerowperind = TRUE,               # one individual per row
                               col.pop = 1,                       # population ID is in the first column
                               ask = FALSE)                       # won't ask you questions :)

snow # This is now a 'genind' object
summary(snow)

# What's stored in a genind object?
attributes(snow)

# $tab is the part that stores the counts for each allele which are used to compute diversity metrics 

## Look at population structure -----
# Sample sites are not necessarily separate "populations." The goal of studies is often to
# describe *population structure* by clustering locations with similar genetic makeup (similar 
# allele frequencies) indicating that they likely make up a single population.

# We can get a rough idea about population structure by doing a quick principal components analysis (PCA) 
# on the allele frequency data:
X <- scaleGen(snow, NA.method = "mean") # scale and center the data; tell it how to handle missing values
pcaX <- dudi.pca(X, scannf=FALSE, scale= F, center = F) # PCA analysis
s.class(pcaX$li, pop(snow)) # plot PCA results

### Q3: What do you see? Describe the population structure of snowshoe hare. ----

## Optional if you have ggplot and the basemap loaded ##

# Populations are generally not so rigid as clustering will suggest. They can be quite 
# arbitrary and depend on what the research question is.
# Looking at higher or lower levels of clustering can help understand the data.
# Try plotting with different numbers of clusters (k):
plot_clusters(data = snow, sf_data = coords_spatial, clusters = 3)

# 3. Estimating diversity metrics (alpha diversity) --------
# We can estimate the genetic diversity at each site in several ways. Here we'll use two: allelic richness and gene diversity.

## Allelic richness ------
# Allelic richness is the total number of alleles at each site. 
# We can see this in the summary of the genind object:
s.snow <- summary(snow)
total_alleles <- s.snow$pop.n.all

hist(total_alleles)

# Let's compare this to the number of individuals sampled at each site:
num_individuals <- s.snow$n.by.pop

plot(total_alleles ~ num_individuals)

### Q4: What's happening in this plot? How correlated are the numbers of alleles and numbers of samples? ----
cor.test(total_alleles, num_individuals)

# In general, allelic richness is not a good measure of genetic diversity unless sample sizes are equal.
# We will use rarefaction to standardize this value according to sample size.

(ar_hares <- allelic.richness(snow, min.n = 14)) # min.n is the lowest number of alleles sampled in a population. 
# This is the default value!
# the smallest sample size is 7 individuals (=14 alleles)

# We can take the mean richness across all loci to get 1 estimate for the population:
ar_hares <- colMeans(ar_hares$Ar)
hist(ar_hares)

# Now let's see the relationship with sample size:
plot(ar_hares ~ num_individuals)

cor.test(ar_hares, num_individuals)

### Q5: what happens if you change the minimum sample size? -----
hist(colMeans(allelic.richness(snow, min.n = 2)$Ar))
hist(colMeans(allelic.richness(snow, min.n = 30)$Ar))


## Gene diversity ----- 
# Gene diversity is the probability of choosing two alleles that are different from the population. 
# This is a metric of diversity based on *evenness*: it is highest when all alleles have the same frequency.
# Gene diversity varies between 0 (low diversity) and 1 (high diversity).
(gd_hares <- Hs(snow))
hist(gd_hares)

# How is gene diversity related to sample size?
plot(gd_hares~num_individuals)

cor.test(gd_hares, num_individuals)

### Q6: How is the gene diversity of these hares? -----

# Compare allelic richness and gene diversity
plot(gd_hares~ar_hares)

cor.test(gd_hares, ar_hares)

# 4. Estimating differentiation metrics (beta diversity) ----
# In population genetics we typically use FST to estimate genetic differentiation. This is based on Wright's F-statistics,
# and it's a way to partition variance in allele frequencies within populations vs between populations.
# There are many flavors of FST. (Technically, the one we use for microsatellite data is called GST).

# A common FST is pairwise FST developed by Weir & Cockerham (1984). 
# With this we compare all pairs of populations.
snow_hst <- genind2hierfstat(snow)

(pw_fst <- hierfstat::pairwise.WCfst(snow)) # this is a bit slow for bigger datasets

# This gives us a matrix of pairwise differentiation where higher numbers indicate stronger differentiation.

# Q7. What are the NA values in the FST matrix?

# We can visualize pairwise FST using principal coordinates analysis (PCoA)
cmd.pair.fst <- cmdscale(dist(pw_fst), k=3, eig=T)
cmd.fst <- cmd.pair.fst$points
cmd.fst12 <- data.frame(pop = rownames(cmd.fst),
                        PCoA1 = cmd.fst[,1],
                        PCoA2 = cmd.fst[,2])

plot(cmd.fst12$PCoA2, cmd.fst12$PCoA1)
## Q7: Does anything seem familiar in this plot? -----
## Hint: population differentiation is a measure of population structure

# Pairwise FST is useful, but sometimes for our analyses it's simpler to have a single value per population.
# And, especially when working with data from multiple species, comparing pairwise FST can be misleading due to way it's calculated
# We can overcome these issues using another metric of differentiation: population-specific FST.
# The interpretation of this metric is slightly different: it is an estimate of how far a population has diverged from the common
# ancestor of all the populations sampled.

ps.fst <- betas(snow)
names(ps.fst)

# Population-specific FSTs are stored in 'betaiovl'
ps.fst <- ps.fst$betaiovl
summary(ps.fst)
hist(ps.fst)


# Let's plot these on the map!
coords_spatial$ps.fst <- ps.fst[order(names(ps.fst))] # populations are in alphabetical order in the coordinate data

ggplot() + 
  geom_sf(data = ca_usa) +
  geom_sf(data = coords_spatial, aes(color = ps.fst), size = 2) +
  coord_sf(crs = 'ESRI:102009') + 
  theme_minimal()

## Q8: Does this map make sense to you? Why or why not?