# install.packages("DataExplorer")
library(DataExplorer)
library(tidyverse)

source("data_loading.R")
real_estate <- load_data_EDA()

# Dużo kategorycznych
# Sporo kolumn typu <int>, które tylko udają że są ciągłe
introduce(real_estate)
plot_intro(real_estate)

# Dużo zależnych kolumn - jak nie ma garażu to jakość garażu to NA itd
real_estate %>%
  select(starts_with("Bsmt")) ->
  basement

real_estate %>%
  select(starts_with("Garage")) -> 
  garage

real_estate %>%
  select(!starts_with("Bsmt")) %>%
  select(!starts_with("Garage")) ->
  real_estate_trimmed

plot_str(list(
  real_estate_trimmed,
  basement,
  garage
))


plot_missing(
  profile_missing(
    real_estate
  )
)

plot_bar(garage)
plot_bar(real_estate, by = "DATASET")

plot_histogram(real_estate)

plot_correlation(real_estate)

plot_scatterplot(real_estate, by=LABEL)
