# Genetic diversity practical part II: Drivers of genetic diversity change
# How does urbanization affect birds and mammal genetic diversity?

# Libraries
library(ggplot2) # plotting

library(lme4)    # modeling


# Load the data -----
# Genetic diversity metrics for mammals (41 species) and birds (25 species) sampled at 1008 locations in the US and Canada.
# Data is from: Schmidt et al. (2020) "Continent-wide effects of urbanization on bird and mammal genetic diversity." Proc B 287(1920)
# paper: https://royalsocietypublishing.org/doi/full/10.1098/rspb.2019.2497#d1e1557
# data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.cz8w9gj0c

# Genetic diversity metrics:
# gene_diversity = gene diversity
# allelic_richness = allelic richness rarefied to a minimum of 10 alleles (the minimum across all datasets)
# global_fst = population-specific FST

# Urbanization variables:
# urban = binary variable; 0 = non-urban, 1 = urban
# popden = human population density
# hfi = Human Footprint Index (0 = most wild; 100 = most transformed)

dat <- read.csv('data/synthesized_geneticdiversity_dryad.csv', header=T)

head(dat)
str(dat)

# Look at the data ------
plot(dat$gene_diversity ~ dat$hfi)
plot(dat$allelic_richness ~ dat$hfi)
plot(dat$global_fst ~ dat$hfi) # Is there anything funny about this plot?

#Q1: how do these plots look for the other urbanization variables, 'urban' and 'popden'? -----

# We can look at a boxplot for the categorical urban variable:
boxplot(gene_diversity~urban, data = dat, notch = TRUE)


# But, there are 66 species in here, in 2 different taxonomic classes.

# Q2: make a boxplot comparing gene diversity of birds vs mammals. Are they different? -----