library(dplyr)
library(naniar)
library(splitstackshape)
library(catboost)
set.seed(123)

catboost_prepare_data <- function(data, target, continuous_names = NULL, drops = NULL, char_na = NULL){
  'This function prepares given data in order to preform catboost modeling on it.
  
   It returns the list of data frames, which are:
    X_train - all independent variables from the sample of 70% of given data , 
              will be used to train the model
    X_test - the rest (30%) of independent variables, 
             which will be used to make predictions to test the accuracy of the model
    y_train - 70% of the records of dependent variable, corresponding to X_train 
    y_test - records of dependent variable, corresponding to X_test, 
            which will be used to test the accuracy between actual values and predictions
  
   Arguments:
    data - the data frame to prepare
    target - targeted variable of the model
    continuous_names - a vector of names of continuous columns in data frame
    drops - a vector of names of highly correlated columns to drop,
            based on correlation matrix
    char_na - character or double used to substitute NA in the data frame'
  
  ## Data cleaning - removing data with nearly all variables blank 
  data <- data %>% mutate_all(funs(replace(., .== char_na, NA))) %>%
    na.omit()
  
  ## Encoding continuous variables by grouping
  
  data[continuous_names] <- data %>%
    select(continuous_names) %>%
    sapply(function(x) {
      q <- quantile(x, probs = c(0.33,0.66,1)) + c(1e-3, 2e-3, 3e-3) # eps to avoid duplicates
      x <- cut(x, breaks = c(-Inf,q[1],q[2],q[3]), 
               labels = c(0,1,2))
    })
  
  ## Encoding character data with integer labels
  data[, sapply(data, class) == 'character'] <- data %>%
    select(where(is.character)) %>%  
    sapply(function(x) as.integer(as.factor(x)) )
  
  ## Remove highly correlated variables
  data <- data[ , !(names(data) %in% drops)]
  
  ## Stratifying the data
  
  all <- stratified(data, c(target), 0.7, bothSets = TRUE)
  X_train <- all$SAMP1
  X_test <- all$SAMP2
  y_train <- X_train %>%
    pull(target) %>%
    as.numeric()
  print(y_train)
  y_test <- X_test %>%
    pull(target) %>%
    as.numeric()
  exclude <- c(target)
  X_train <- select(X_train, -exclude) %>% sapply(function(x){as.numeric(x)})
  X_test <- select(X_test, -exclude) %>% sapply(function(x){as.numeric(x)})
  
  list <- list("X_train" = X_train, "X_test" = X_test, "y_train" = y_train, "y_test" = y_test)
  
  return(list)
}

catboost_function <- function(X_train,y_train, params=list(loss_function = 'RMSE')){
  'This function builds the catboost model with default parameters.
   The outcome of the function is mentioned model.
  
   Arguments:
    X_train - all independent variables from the sample of 70% of given data , 
              will be used to train the model, should be prepared by the function 
              `catboost_prepare_data`
    y_train - 70% of the records of dependent variable, corresponding to X_train,
              should be prepared by the function `catboost_prepare_data`
    params - list of parameters to base the model on, as for this version: list(loss_function = "RMSE") by changing 
              loss_funtion you can choose what tipe of problem it is going to be solved:
              for example (regression: "RMSE", binar classification: "Logloss", multiclassification: "MultiClass")'
  
  ## Build the model
  train_pool <- catboost.load_pool(data = X_train, label = y_train)
  model <- catboost.train(train_pool, params = params)
  
  return(model)
}

catboost_predict <- function(model, X_test, y_test = NULL, type = 'RawFormulaVal'){
  'This function returns the predicitons of given data set, based on the used model
  
   Arguments:
    model - desired catboost model built with `catboost_function`
    X_test - the rest (30%) of independent variables, 
             which will be used to make predictions to test the accuracy of the model,
             should have been prepared with `catboost_prepare_data`
    y_test - y_test - records of dependent variable, corresponding to X_test, 
            which will be used to test the accuracy between actual values and predictions,
            should have been prepared with `catboost_prepare_data`
    type - for classification and multiclassification use "Class"
  '
  
  
  ## Predict the results from X_test
  
  test_pool <- catboost.load_pool(data = X_test, label = y_test)
  
  predict <- catboost.predict(model, 
                              test_pool,
                              prediction_type = type)
  
  return(as.vector(predict))
}


cat_data <- load_transform()$train %>%
  select(-Id) %>%
  missing_fill() %>%
  catboost_prepare_data(
    LABEL,
    continuous_names = CONTINUOUS_FEATURES
  )

catboost_grid <- expand.grid(
  iterations = c(100, 500, 1000),
  depth = c(3, 4, 6, 8, 10),
  l2_leaf_reg = c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5),
  learning_rate = c(1e-1, 1e-2, 1e-3),
  rsm = 0.95,
  border_count = 254
)

catboost_fit_control <- caret::trainControl(
  method = "cv",
  number = 4,
  search = "grid"
)

report <- caret::train(
  x = data.frame(cat_data$X_train),
  y = cat_data$y_train,
  method = catboost.caret,
  preProc = NULL,
  tuneGrid = catboost_grid, 
  trControl = catboost_fit_control)

plot_box <- function(results, which) {
  which <- enquo(which)
  results %>%
    select(!!which, RMSE) %>%
    mutate(across(!!which, as.factor)) %>%
    ggplot() +
    geom_boxplot(aes(x=!!which, y=RMSE))
}

plot_vio <- function(results, which) {
  which <- enquo(which)
  results %>%
    select(!!which, RMSE) %>%
    mutate(across(!!which, as.factor)) %>%
    ggplot() +
    geom_violin(aes(x=!!which, y=RMSE))
}

res1 <- report$results

res1 %>% write.csv("catboost_models/res1.csv", row.names = F)

res1 <- read_csv("catboost_models/res1.csv")

plot_box(res1, depth)
plot_box(res1, learning_rate)
plot_box(res1, l2_leaf_reg)
plot_box(res1, iterations)


catboost_grid2 <- expand.grid(
  iterations = 1:25 * 100,
  depth = c(3, 6),
  l2_leaf_reg = 1e-3,
  learning_rate = 1e-1,
  rsm = 0.95,
  border_count = 254
)

report2 <- caret::train(
  x = data.frame(cat_data$X_train),
  y = cat_data$y_train,
  method = catboost.caret,
  preProc = NULL,
  tuneGrid = catboost_grid2, 
  trControl = catboost_fit_control)

saveRDS(report2, "catboost_models/report2.RDs")
report2 <- readRDS("catboost_models/report2.RDs")
ggplot(report2)

catboost_random_grid <- expand.grid(
  iterations = 5:15 * 100,
  depth = 3:10,
  l2_leaf_reg = c(1e-1, 5e-2, 1e-2, 5e-3, 1e-3),
  learning_rate = 1e-1,
  rsm = 15:20 * 0.05,
  border_count = 254
)

catboost_random_control <- caret::trainControl(
  method = "cv",
  number = 4,
  search = "grid"
)

report3 <- caret::train(
  x = data.frame(cat_data$X_train),
  y = cat_data$y_train,
  method = catboost.caret,
  preProc = NULL,
  tuneGrid = catboost_random_grid[sample(nrow(catboost_random_grid), 100),], 
  trControl = catboost_random_control)

res3 <- report3$results
res3 %>% write_csv("catboost_models/res3.csv")
res3 <- read_csv("catboost_models/res3.csv")

plot_box(res3, depth)
plot_box(res3, iterations)
plot_box(res3, rsm)
plot_vio(res3, rsm)
plot_box(res3, l2_leaf_reg)
plot_vio(res3, l2_leaf_reg)

res3 %>% arrange(RMSE) %>% head()

#  depth learning_rate l2_leaf_reg  rsm border_count iterations     RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
# 1     5           0.1       0.050 0.75          254        800 29861.83 0.8767981 18322.30 4797.986 0.03593863 1308.916
# 2     5           0.1       0.050 0.75          254        700 29874.63 0.8767164 18306.95 4804.149 0.03604708 1292.154
# 3     5           0.1       0.050 0.75          254        500 29890.91 0.8767092 18274.68 4757.162 0.03562553 1234.829
# 4     4           0.1       0.010 0.90          254       1400 29981.29 0.8748630 18319.07 6360.117 0.04919365 1906.038
# 5     6           0.1       0.010 0.90          254       1200 30033.95 0.8749986 18141.71 6348.159 0.04955098 1658.444
# 6     6           0.1       0.005 0.90          254        600 30082.77 0.8747703 18164.98 5980.348 0.04702807 1679.126

m1 <- catboost_function(
  cat_data$X_train,
  cat_data$y_train,
  params=list(
    depth=5,
    learning_rate=0.1,
    l2_leaf_reg=0.05,
    rsm=0.75,
    border_count=254,
    iterations=800,
    loss_function = 'RMSE'
  ))

RMSE(cat_data$y_train, catboost_predict(m1, cat_data$X_train)) # 3312.41
RMSE(cat_data$y_test, catboost_predict(m1, cat_data$X_test)) # 24471.06

m2 <- catboost_function(
  cat_data$X_train,
  cat_data$y_train,
  params=list(
    depth=5,
    learning_rate=0.1,
    l2_leaf_reg=0.05,
    rsm=0.75,
    border_count=254,
    iterations=700,
    loss_function = 'RMSE'
  ))

RMSE(cat_data$y_train, catboost_predict(m2, cat_data$X_train)) # 3906.97
RMSE(cat_data$y_test, catboost_predict(m2, cat_data$X_test)) # 24483.43


m3 <- catboost_function(
  cat_data$X_train,
  cat_data$y_train,
  params=list(
    depth=5,
    learning_rate=0.1,
    l2_leaf_reg=0.05,
    rsm=0.75,
    border_count=254,
    iterations=500,
    loss_function = 'RMSE'
  ))

RMSE(cat_data$y_train, catboost_predict(m3, cat_data$X_train)) # 5604.793
RMSE(cat_data$y_test, catboost_predict(m3, cat_data$X_test)) # 24563.16

m4 <- catboost_function(
  cat_data$X_train,
  cat_data$y_train,
  params=list(
    depth=4,
    learning_rate=0.1,
    l2_leaf_reg=0.01,
    rsm=0.90,
    border_count=254,
    iterations=1400,
    loss_function = 'RMSE'
  ))

RMSE(cat_data$y_train, catboost_predict(m4, cat_data$X_train)) # 3280.824
RMSE(cat_data$y_test, catboost_predict(m4, cat_data$X_test)) # 25975.99

m5 <- catboost_function(
  cat_data$X_train,
  cat_data$y_train,
  params=list(
    depth=6,
    learning_rate=0.1,
    l2_leaf_reg=0.01,
    rsm=0.90,
    border_count=254,
    iterations=1200,
    loss_function = 'RMSE'
  ))

RMSE(cat_data$y_train, catboost_predict(m5, cat_data$X_train)) # 397.5021
RMSE(cat_data$y_test, catboost_predict(m5, cat_data$X_test)) # 24430.67

saveRDS(m1, "catboost_models/m1.RDS")
m1 <- readRDS("catboost_models/m1.RDS")

wykres(cat_data$y_test, catboost_predict(m1, cat_data$X_test))

saveRDS(m2, "catboost_models/m2.RDS")

m2 <- readRDS("catboost_models/m2.RDS")

wykres(cat_data$y_test, catboost_predict(m2, cat_data$X_test))

saveRDS(m3, "catboost_models/m3.RDS")
saveRDS(m4, "catboost_models/m4.RDS")
saveRDS(m5, "catboost_models/m5.RDS")
