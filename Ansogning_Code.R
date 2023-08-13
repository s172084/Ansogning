
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
gravier_long <- pivot_longer( data = gravier_clean,
                                   cols = ! starts_with("o"),
                                   names_to = "gene")


gravier_long

# Choose 100 random genes. 
gravier_long_s <- sample_n(gravier_long,100)
gravier_long_s 

gravier_long_sren <- rename(gravier_long_s ,log2_expr_level = value)
gravier_long_sren


# Fit a logistic regression to each gene, modelling: “outcome ~ log2_expr_lvl”
# *** Its correct until here... (Sleep)***

gravier_data_nested <-gravier_data_nested %>%
  mutate(model = map(data, ~ glm(outcome ~ log2_expr_level,
                                 gravier_data_nested
                                 data = .,
                                 family = binomial(link = "logit"))))

gravier_data_nested <- gravier_data_nested %>%
  # for each model, generate tidy data with estimate, std.error, statistic, p-value
  mutate(tidied_model = map(model, tidy, conf.int = TRUE)) %>% # take the tidied model out of a tibble and show it.
  unnest(tidied_model)
gravier_data_nested


gravier_data_long_nested <- gravier_data_nested %>%
  filter(str_detect(term,"log2_expr_level"))
gravier_data_long_nested

# Add beta-estimates and confidence intervals
# Add an indicator for whether the p-value was less than or equal to 0.05
# Add code to the README showing short analysis with nice clear code.
# That is your long modelled data,create a forest-plot of the slopes (beta1 estimates) 
# and add 95% CI
