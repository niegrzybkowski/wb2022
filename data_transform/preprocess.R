library(tidyverse)

load_transform <- function() {
  list(
    train=load_data_train() %>%
      full_transform(),
    test=load_data_test() %>%
      full_transform()
  )
}

full_transform <- function(data) {
  data %>%
    num_var_transform() %>%
    qual_var_transform(QUALITY_FEATURES) %>%
    other_var_transform() %>%
    datatype_transform()
}

LABEL <- "SalePrice"

QUALITY_FEATURES <- c(
  "ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "HeatingQC",
  "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PoolQC")

EVALUATION_FEATURES <- c(
  QUALITY_FEATURES, "BsmtExposure", "BsmtFinType1", "BsmtFinType2",
  "Functional", "OverallQual", "OverallCond")

CONTINUOUS_FEATURES <- c(
  "WoodDeckSF", "OpenPorchSF", "GarageArea", "EnclosedPorch",  "PoolArea",
  "X2ndFlrSF", "X1stFlrSF", "TotalBsmtSF", "GrLivArea", "YearRemodAdd",
  "YearBuilt", "MasVnrArea", "LotFrontage", "LotArea", "BsmtUnfSF", 
  "BsmtFinSF1", "BsmtFinSF2")

CATEGORICAL_FEATURES <- c(
  "MSSubClass", "MSZoning", "Street", "Alley", "LotShape", "LandContour", 
  "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", 
  "Condition2", "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", 
  "Exterior1st", "Exterior2nd", "MasVnrType", "Foundation", "Heating", 
  "CentralAir", "Electrical", "LowQualFinSF", "BsmtFullBath", "BsmtHalfBath", 
  "FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", 
  "Fireplaces", "GarageType", "GarageYrBlt", "GarageFinish", "GarageCars", 
  "PavedDrive", "X3SsnPorch", "ScreenPorch", "Fence", "MiscFeature", "MiscVal", 
  "MoSold", "YrSold", "SaleType", "SaleCondition")

save_data <- function()

load_data_train <- function() {
  read.csv("data/train.csv") %>% tibble()
}

load_data_test <- function() {
  read.csv("data/test.csv") %>% tibble()
}

num_var_transform <- function(data) {
  data %>% 
    mutate(MSSubClass = 
             case_when(
               MSSubClass == 20 ~ "1-STORY 1946 & NEWER ALL STYLES",
               MSSubClass == 30 ~ "1-STORY 1945 & OLDER",
               MSSubClass == 40 ~ "1-STORY W/FINISHED ATTIC ALL AGES",
               MSSubClass == 45 ~ "1-1/2 STORY - UNFINISHED ALL AGES",
               MSSubClass == 50 ~ "1-1/2 STORY FINISHED ALL AGES",
               MSSubClass == 60 ~ "2-STORY 1946 & NEWER",
               MSSubClass == 70 ~ "2-STORY 1945 & OLDER",
               MSSubClass == 75 ~ "2-1/2 STORY ALL AGES",
               MSSubClass == 80 ~ "SPLIT OR MULTI-LEVEL",
               MSSubClass == 85 ~ "SPLIT FOYER",
               MSSubClass == 90 ~ "DUPLEX - ALL STYLES AND AGES",
               MSSubClass == 120 ~ "1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
               MSSubClass == 150 ~ "1-1/2 STORY PUD - ALL AGES",
               MSSubClass == 160 ~ "2-STORY PUD - 1946 & NEWER",
               MSSubClass == 180 ~ "PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
               MSSubClass == 190 ~ "2 FAMILY CONVERSION - ALL STYLES AND AGES"
             ))
}

qual_var_transform <- function(data, vars) {
  for (i in 1:length(vars)){
    n <- vars[i]
    name <- data[[n]]
    name <- case_when(
      is.na(name) ~ -1,
      name == "Ex" ~ 5,
      name == "Gd" ~ 4,
      name == "TA" ~ 3,
      name ==  "Fa" ~ 2,
      name == "Po" ~ 1,
      name == "Absent" ~ -1
    )
    data[n] <- name
  }
  data
}

other_var_transform <- function(data) {
  data <- data %>% 
    mutate(BsmtExposure = case_when(
      BsmtExposure == "Gd" ~ 3,
      BsmtExposure == "Av" ~ 2,
      BsmtExposure == "Mn" ~ 1,
      BsmtExposure == "No" ~ 0,
      BsmtExposure == "Absent" ~ -1
    ))
  
  data <- data %>% 
    mutate(BsmtFinType1 = case_when(
      BsmtFinType1 == "GLQ" ~ 5,
      BsmtFinType1 == "ALQ" ~ 4,
      BsmtFinType1 == "BLQ" ~ 3,
      BsmtFinType1 == "Rec" ~ 2,
      BsmtFinType1 == "LwQ" ~ 1,
      BsmtFinType1 == "Unf" ~ 0,
      BsmtFinType1 == "Absent" ~ -1
    )) 
  
  data <- data %>% 
    mutate(BsmtFinType2 = case_when(
      BsmtFinType2 == "GLQ" ~ 5,
      BsmtFinType2 == "ALQ" ~ 4,
      BsmtFinType2 == "BLQ" ~ 3,
      BsmtFinType2 == "Rec" ~ 2,
      BsmtFinType2 == "LwQ" ~ 1,
      BsmtFinType2 == "Unf" ~ 0,
      BsmtFinType2 == "Absent" ~ -1
    )) 
  
  data <- data %>% 
    mutate(Functional = case_when(
      Functional == "Typ" ~ 7,
      Functional == "Min1" ~ 6,
      Functional == "Min2" ~ 5,
      Functional == "Mod" ~ 4,
      Functional == "Maj1" ~ 3,
      Functional == "Maj2" ~ 2,
      Functional == "Sev" ~ 1,
      Functional == "Sal" ~ 0,
    ))
  data
}

datatype_transform <- function(data) {
  data %>%
    mutate(across(any_of(CONTINUOUS_FEATURES), as.numeric))
}
