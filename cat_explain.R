source("catboost_simple.R")

library(DALEX)

m1ex <- DALEX::explain(
  m1, 
  data = cat_data$X_train,
  y = cat_data$y_train,
  predict_function = catboost_predict,
  label="cat1"
)
m2ex <- DALEX::explain(
  m2, 
  data = cat_data$X_train,
  y = cat_data$y_train,
  predict_function = catboost_predict,
  label="cat2"
)
m3ex <- DALEX::explain(
  m3, 
  data = cat_data$X_train,
  y = cat_data$y_train,
  predict_function = catboost_predict,
  label="cat3"
)
m4ex <- DALEX::explain(
  m4, 
  data = cat_data$X_train,
  y = cat_data$y_train,
  predict_function = catboost_predict,
  label="cat4"
)

m5ex <- DALEX::explain(
  m5, 
  data = cat_data$X_train,
  y = cat_data$y_train,
  predict_function = catboost_predict,
  label="cat5"
)

m1mp <- model_parts(m1ex)
m2mp <- model_parts(m2ex)
m3mp <- model_parts(m3ex)
m4mp <- model_parts(m4ex)
m5mp <- model_parts(m5ex)


plot(m1mp, m2mp, m3mp, m4mp, m5mp, max_vars=5)
plot(m2mp, max_vars=20)

m1pf <- model_profile(m1ex)
m2pf <- model_profile(m2ex)

plot(m1pf, max_vars=20)

lm1 <- lm(m2.price ~ ., data=DALEX::apartments)
lm1ex <- DALEX::explain(
  lm1, 
  data=DALEX::apartments[,2:6],
  y=DALEX::apartments[,1]
)
library(ranger)
lm1mp <- model_parts(lm1ex)
plot(lm1mp, lm1mp)
