library(ggplot2)
library(Metrics)

RMSE <- function(actual, pred) {
  sqrt(colMeans((actual - pred)^2))
}

MAE<- function(actual, pred) {
  mae(actual, pred)
}


plot <- function(actual, pred){
  value <- (pred/actual-1)*100


  
  data <- data.frame(
    id= 1:lengths(value),  
    values=unlist(value)
  )
  plot <- ggplot(data, aes(x=id, y=values)) + 
    geom_bar(stat = "identity")+
    ggtitle("Ró¿nica procentowa pomiêdzy wartoœciami z predykacji, a rzeczywsitymi")+
    xlab("Obserwacje")+
    ylab("Ró¿nica procentowa")
  
  plot
}
