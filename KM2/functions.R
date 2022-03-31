library(lightgbm)


lightGBM_function <- function(data, target, 
                              num_leaves = 4L,
                              learning_rate = 1.0,
                              objective = "multiclass", 
                              nthread = 2L
){
  
  
  #Gdy nie regresja to trzeba zamienić aby było od 0
  if (objective != "regression") {
    data[, target] <- as.numeric(as.factor(data[, target])) - 1L
  }
  
  
  
  #Gdy jest ramka danych, przerabiamy ją na coś pasującego do lightGBM
  if ("data.frame" %in% class(data)) {
    
    #Wyciągnięcie targetu i zamiana na macierz ramek
    target_val <- data[, target]
    data <- data[ , !(names(data) %in% target)]
    data <- as.matrix(data[, 1:ncol(data)])
    
    #Tworzenie lgb.datasets treningoego
    data <- lgb.Dataset(
      data = data,
      label = target_val,
      free_raw_data = FALSE
    )
  }
  
  
  
  if (objective == "multiclass") {
    params <- list(
      objective = "multiclass"
      , metric = "multi_error"
      , num_class = length(unique(target_val))
      , min_data = 1L
      , learning_rate = 1.0,
      nthread = nthread
    )
  }
  else{
    params <- list(
      num_leaves = num_leaves
      , learning_rate = learning_rate
      , objective = objective
      , nthread = nthread
    )
  }
  
  
  
  params <- list(
    objective = "multiclass"
    , metric = "multi_error"
    , num_class = length(unique(target_val))
    , min_data = 1L
    , learning_rate = 1.0
  )
  
  
  # Model
  model <- lgb.train(
    params   = params
    , data = data
    , nrounds =  100L
  )
  
  return(model)
}


lightGBM_predict <- function(model, data){
  
  test <- as.matrix(data)
  
  return(predict(model, test))
}


## Przykład użycia

data(iris)

split1 <- sample(c(rep(0, 0.7 * nrow(iris)), rep(1, 0.3 * nrow(iris))))
train <- iris[split1 == 0, ]  
model <- lightGBM_function(train, "Species" )


test <- iris[split1== 1, ]
test <- test[,1:4]
pred <- lightGBM_predict(model, test)




