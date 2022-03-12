library(tidyverse)
LABEL <- "SalePrice"

load_data_train <- function() {
  read.csv("data/train.csv") %>% tibble()
}

load_data_test <- function() {
  read.csv("data/test.csv") %>% tibble()
}

load_data_EDA <- function() {
  train <- load_data_train();
  train["DATASET"] <- "Train";
  test <- load_data_test();
  test[LABEL] <- NA;
  test["DATASET"] <-  "Test";
  rbind(train, test) %>% tibble()
}

