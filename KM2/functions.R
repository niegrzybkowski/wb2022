
#Na razie parametry domyślne wrzucam po prostu do argumentów w funkcji.
lightGBM_function <- function(data, target, ...,  num_leaves = 4L,
                              learning_rate = 1.0,objective = "binary", nthread = 2L){
  
  #Podział danych na treningowy i testowy (opcjonalnie?)
  split1 <- sample(c(rep(0, 0.7 * nrow(data)), rep(1, 0.3 * nrow(data))))
  train <- data[split1 == 0, ]  
  test <- data[split1== 1, ]
  
  #Gdy jest ramka danych, przerabiamy ją na coś pasującego do lightGBM
  if (class(data) == "data.frame") {
    rules <- lgb.convert_with_rules(data = train)
    bank_train <- rules$data
    test <- lgb.convert_with_rules(data = test, rules = test$rules)$data
    
    # Jak mamy regresję, to nie robimy tego kroku, bo to tylko
    # normalizuje klasy w przypadku klasyfikacji, tak żeby rozpoczynały się
    # od wartości 0
    if (objective != "regression") {
      train$y <- train$target - 1L
      train$y <- test$target - 1L
    }
    
    #Wyciągnięcie targetu i zamiana na macierz ramek
    target_train <- train$target
    target_test <- test$target
    train <- train[, -c(target)]
    test <- test[, -c(test)]
    
    train <- as.matrix(train[, 1:ncol(train), with = FALSE])
    test <- as.matrix(test[, 1:ncol(test), with = FALSE])
  }
  
  #Tworzenie lgb.datasets treningoego i walidaycjnego 
  dtrain <- lgb.Dataset(
    data = train
    , label = target_train
  )
  dtest <- lgb.Dataset.create.valid(
    dtrain
    , data = test
    , label = target_test
  )

  # Parametry w liście, tak jakby wykorzystuje te defaultowe ale
  # to trzeba inaczej raczej
  train_params <- list(
    num_leaves = num_leaves
    , learning_rate = learning_rate
    , objective = objective
    , nthread = nthread
  )
  
  #Jak jest multiclass ti chyba trzeba podać ilość klas, więc
  # dodajemy num_class to train_params
  if (objective == 'multiclass') {
    tmp <- list(c(num_class = length(unique(target))))
    params_list <- append(params_list, num_class)
  }
  
  # Model
  model <- lightgbm(
    data = dtrain
    , params = train_params
    , nrounds = 2L
    , valids = list(train = dtrain, valid = dtest)
  )
  
  
  #Absolutnie nie wiem czy to działa :')
  return(model)
}

