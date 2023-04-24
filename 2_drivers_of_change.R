# Genetic diversity practical part II: Drivers of genetic diversity change
# How does urbanization affect birds and mammal genetic diversity?

# Libraries
library(ggplot2)  # plotting

library(lme4)     # modeling
library(lmerTest) # modeling

source('other_functions.R')

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

# 1. Data exploration ------
plot(dat$gene_diversity ~ dat$hfi)
plot(dat$allelic_richness ~ dat$hfi)
plot(dat$global_fst ~ dat$hfi) # Is there anything funny about this plot?

##Q1: how do these plots look for the other urbanization variables, 'urban' and 'popden'? -----

# We can look at a boxplot for the categorical urban variable:
boxplot(gene_diversity~urban, data = dat, notch = TRUE) # What are the notches on boxplots?


# But, there are 66 species in here, in 2 different taxonomic classes.

## Q2: make a boxplot comparing gene diversity of birds vs mammals. Are they different? -----

# What about urban vs nonurban birds and mammals?
boxplot(gene_diversity~urban + class, data = dat, notch = TRUE)

# We'll subset birds and mammals and deal with them separately
mammals <- subset(dat, class == 'mammal')
birds <- subset(dat, class == 'bird')

# Let's see whether all species have similar relationships with the Human Footprint Index:
ggplot(mammals, aes(y = gene_diversity, x = hfi, color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = NA) + # add best fit lines per species
  guides(color = 'none') +                # remove legend for now
  theme_minimal()

# vs lumping all species:
ggplot(mammals, aes(y = gene_diversity, x = hfi)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = NA) + # add best fit lines per species
  guides(color = 'none') +                # remove legend for now
  theme_minimal()

# Repeat for birds:
ggplot(birds, aes(y = gene_diversity, x = hfi, color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = NA) +
  guides(color = 'none') +
  theme_minimal()

ggplot(birds, aes(y = gene_diversity, x = hfi)) +
  geom_point() +
  geom_smooth(method = 'lm', fill = NA) +
  guides(color = 'none') +
  theme_minimal()

# There are differences across classes and across species that we should account for in our models.

# 2. Testing effects of urbanization on genetic diversity -----
# For this we will use hierarchical linear models (AKA linear mixed models)
mammals$sc_gd <- scale(mammals$gene_diversity)
mammals$sc_ar <- scale(mammals$allelic_richness)

mammals$sc_hfi <- scale(mammals$hfi)
mammals$sc_pop <- scale(log(mammals$popden+1))

# Run a model looking at the effect of the Human Footprint Index on gene diversity:
gd_mod <- lmer(sc_gd ~ sc_hfi + (sc_hfi||species), data = mammals)
summary(gd_mod)

ar_mod <- lmer(sc_ar ~ sc_hfi + (sc_hfi||species), data = mammals)
summary(ar_mod)

# Check the model residuals:
res <- residuals(gd_mod)
plot(res)
qqnorm(res)
qqline(res)

## Q3: Does the Human Footprint Index affect gene diversity? ------

## Q4: Are effects consistent across alpha diversity metrics?  -----
# Try to test the effects of other urbanization variables on gene diversity and allelic richness. 
# What do you predict you'll find? Do your predictions hold?

## Q5: How does urbanization affect beta diversity (differentiation)?  -----
# Try to test the effects of other urbanization variables on population-specific FST
# called *global_fst* in this dataset.
# What do you predict you'll find? Do your predictions hold?

# 3. Plot results ------

# Create a list of models
model_list <- list(gd_mod, ar_mod)

# Make a plot for 1 predictor variable (here HFI):
predictor_list <- as.list(rep('sc_hfi', length(model_list))) # list of predictors

response_list <- list('gene_diversity', 'allelic_richness', 'FST')

# Extract summaries from all models at once
x <- Map(function(mod, var, resp) plot_format(model = mod, predictor = var, response = resp),
         mod = model_list, var = predictor_list, resp = response_list)

plotdata <- do.call('rbind', x) # combine the data into a single dataframe

## Make the plot:
ggplot(data = plotdata, aes(x = response_var, color = response_var)) +
  geom_hline(yintercept = 0, lty = 'dashed', color = 'gray70', lwd = 0.6) +
  geom_jitter(aes(y = species_effect), width = 0.1, size = 2, alpha = 0.4) +
  geom_pointrange(aes(y = main_effect, ymin = lower_95, ymax = upper_95),
                  size = 0.75, fatten = 5) +
  coord_flip() +
  labs(x = '', y = 'model coefficients') +
  theme_minimal()