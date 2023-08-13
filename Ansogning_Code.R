
library(tidyverse)
# Load the gravier data 
# Note: It is a list
load(file = "data/_raw/gravier.RData")

# Create a tibble from the gravier data. 
ycol <- as_tibble(gravier$y)
xmat <- gravier$x
grav <- bind_cols(ycol, xmat)

# Relocate the y column to the first column and name it outcome.
gravier_not_dirty <- rename(grav, outcome = value)
gravier_not_dirty 

# Recode the outcome so that 0 is good and 1 is poor.
gravier_clean <- gravier_not_dirty %>% 
  mutate(outcome = case_when(outcome == "good" ~ 0,
                             outcome == "poor" ~ 1))

gravier_clean 
View(gravier_clean)

# From clean data, transform to long format. 

# Choose 100 random genes. 

# Fit a logistic regression to each gene, modelling: “outcome ~ log2_expr_lvl”

# Add beta-estimates and confidence intervals

# Add an indicator for whether the p-value was less than or equal to 0.05

# Add code to the README showing short analysis with nice clear code.

# That is your long modelled data,create a forest-plot of the slopes (beta1 estimates) 
# and add 95% CI

# Write data
write_tsv(gravier_clean, file = "data/01_gravier_clean.tsv.gz")


