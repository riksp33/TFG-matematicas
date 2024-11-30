roc_plot = function(Y, X){

  # Combinar los casos y controles en un solo vector
  pred <- c(X, Y)
  
  # Etiquetas: 1 para casos, 0 para controles
  clases <- c(rep(1, length(X)), rep(0, length(Y)))
  
  # Crear la curva ROC
  library(pROC)
  roc_obj <- roc(clases, pred)
  
  # Graficar
  plot(roc_obj, col = "blue", main = "Curva ROC")
}



get_auc = function(Y,X){
  n = length(Y)
  auc = as.numeric(pROC::auc(response = c(rep(1, n), rep(0, n)),
                             predictor = c(Y, X)))
}