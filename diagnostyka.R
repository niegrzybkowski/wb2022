library(ggplot2)
library(Metrics)



MAE<- function(actual, pred) {
  mae(actual, pred)
}


wykres <- function(actual, pred){
  value <- (pred/actual-1)*100
  
  
  
  data <- data.frame(
    id= 1:length(value),  
    values=value
  )
  plot <- ggplot(data, aes(x=id, y=values)) + 
    geom_bar(stat = "identity")+
    ggtitle("Ró¿nica procentowa pomiêdzy wartoœciami z predykacji, a rzeczywsitymi")+
    xlab("Obserwacje")+
    ylab("Ró¿nica procentowa")
  
  plot
}
