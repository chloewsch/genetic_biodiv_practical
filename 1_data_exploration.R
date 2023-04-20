library(tidyr)
library(dplyr)

library(adegenet)

# 1. Exploring a microsatellite dataset --------------
# Data is from: Cheng et al. 2014 "Conservation implications of the evolutionary history and genetic diversity hotspots of the snowshoe hare"
# paper: https://onlinelibrary.wiley.com/doi/full/10.1111/mec.12790
# data: https://datadryad.org/stash/dataset/doi:10.5061/dryad.dh63p

# Microsatellite genotypes for snowshoe hare sampled across their geographic distribution
# Read in the genetic data file:
l_americanus <- read.structure('data/Cheng_Lepus_americanus.str') 

# What happened? Did it work?

# We need more info about the samples before loading the data.
# First, we read it in as a table:
temp <- read.table('data/Cheng_Lepus_americanus.str')

# These data are formatted in a file type (str) for a software called STRUCTURE. 
# In it, each row is an individual, and each column is an allele at a specific locus. 
# Except for the first column: this is a group ID (here, groups are sample locations).
# Snowshoe hares are diploid (two copies of each chromosome), so each locus has 2 columns.
# The columns are basically: ID; Locus 1 allele 1; locus 1 allele 2; locus 2 allele 1, etc.

## Q1: how many individuals are there in this file? -----
# Hint:
nrow(temp)

## Q2: how many loci are there in this file? -----
# Hint:
ncol(temp)

# Read the data in:
l_americanus <- read.structure('data/Cheng_Lepus_americanus.str',  # file
                               n.ind = nrow(temp),                 # number of individuals
                               n.loc = (ncol(temp)-1)/2,           # number of loci
                               onerowperind = TRUE,                # one individual per row
                               col.pop = 1,                        # population ID is in the first column
                               ask = FALSE)                        # won't ask you questions :)


# Look at population structure -----
# Sample sites are not necessarily separate "populations." The goal of many studies is often to describe *population structure* by clustering
# locations with similar genetic makeup (similar allele frequencies) indicating that they likely make up a single population.

# We can get a rough idea about population structure by doing a quick principal components analysis (PCA) on the allele frequency data:
X <- scaleGen(l_americanus, NA.method = "mean") # scale and center the data; tell it how to handle missing values
pcaX <- dudi.pca(X, scannf=FALSE, scale= F, center = F) # PCA analysis
s.class(pcaX$li, pop(l_americanus)) # plot PCA results

## Q3: What do you see? Describe the population structure of snowshoe hare. ----
