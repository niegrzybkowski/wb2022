library(lightgbm)

#' LightGBM_function - Train LightGBM model
#'
#' @param data  data frame object, containing columns of any kind,
#' including variable to predict.
#' @param target  target variable column name
#' @param ...  additional model hyperparameters in format (arg1 = val1,
#'  arg2 = val2) etc.
#'  For list of possible parameters
#'  visit: https://lightgbm.readthedocs.io/en/latest/Parameters.html
#'
#' @return LightGBM model trained on data given as a parameter.
#' @export
#'
#' @examples
#' data(iris)
#' split1 <- sample(c(rep(0, 0.7 * nrow(iris)), rep(1, 0.3 * nrow(iris))))
#' train <- iris[split1 == 0, ]
#' model <- lightGBM_function(train, "Species",
#'                              objective = 'multiclass',
#'                               num_class = 3)


lightGBM_function <- function(data, target, ...){
  
  params <- list(...)
  
  # Tutaj zabiezpeczenie przed niepodaniem argumenty objective
  # Jeśli go nie będzie, to domyślnie model klasyfikacji binarnej
  if (is.null(params$objective)) {
    params <- append(params, list(objective = 'binary'))
  }
  
  
  #Gdy nie regresja to trzeba zamienić aby klasy były numeryczne i od 0
  if (!is.null(params$objective) && params$objective != "regression") {
    data[ ,target] <- as.numeric(as.factor(data[ ,target])) - 1L
  }
  
  
  
  #W sumie faktycznie można założyć, że tylko ramki danych przyjmujemy,
  #bo za dużo zabawy.
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
  
  # if (objective == "multiclass") {
  #   params <- list(
  #     objective = "multiclass"
  #     , metric = "multi_error"
  #     , num_class = length(unique(target_val))
  #     , min_data = 1L
  #   )
  # }
  # else{
  #   params <- list(
  #     num_leaves = num_leaves
  #     , learning_rate = learning_rate
  #     , objective = objective
  #     , nthread = nthread
  #   )
  # }
  
  # Model
  model <- lgb.train(
    params = params
    , data = data
    , nrounds =  100L
  )
  
  return(model)
}


##############################################

#' LightGBM_predict - predict target for samples in data.
#'
#' @param model Trained LightGBM model
#' @param data Data frame to perform prediction on
#'
#' @return vector od predicted values
#' @export
#'
#' @examples
#' data(iris)
#' split1 <- sample(c(rep(0, 0.7 * nrow(iris)), rep(1, 0.3 * nrow(iris))))
#' train <- iris[split1 == 0, ]
#' model <- lightGBM_function(train, "Species",
#'                              objective = 'multiclass',
#'                               num_class = 3)
#' test <- iris[split1== 1, ]
#' test <- test[,1:4]
#' pred <- lightGBM_predict(model, test)

LightGBM_predict <- function(model, data){
  
  test <- as.matrix(data)
  
  return(predict(model, test))
}

#################################################





## Przykład użycia

data(iris)

split1 <- sample(c(rep(0, 0.7 * nrow(iris)), rep(1, 0.3 * nrow(iris))))
train <- iris[split1 == 0, ]  
model <- lightGBM_function(train, "Species" )


test <- iris[split1== 1, ]
test <- test[,1:4]
pred <- lightGBM_predict(model, test)
