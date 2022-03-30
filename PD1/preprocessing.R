library(dplyr)

num_var_transform <- function(data) {
  data <- data %>% 
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
  
  real_estate <- real_estate %>% 
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
}