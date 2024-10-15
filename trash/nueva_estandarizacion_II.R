m = 10000
l = 0.00001
u = 0.99999
roc_plot = function(Y, X, m){
  
  # Combinar los casos y controles en un solo vector
  pred <- c(X, Y)
  
  # Etiquetas: 1 para casos, 0 para controles
  clases <- c(rep(1, length(X)), rep(0, length(Y)))
  
  # Crear la curva ROC
  library(pROC)
  roc_obj <- roc(clases, pred)
  
  # Graficar
  plot(roc_obj, col = "blue", main = paste("Curva ROC: auc = ", m))
}



get_auc = function(Y,X){
  n = length(Y)
  auc = as.numeric(pROC::auc(response = c(rep(1, n), rep(0, n)),
                             predictor = c(Y, X)))
}
eta_poblacional_II = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 2.5, sqrt(0.09))
  numerador = dnorm(inv, mux, sqrt(0.25))
  denominador = dnorm(inv, 2.5, sqrt(0.09))
  
  roc = 1 - pnorm(inv, mux, sqrt(0.25))
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
  
}

eta_pob_sd_stable = function(roc, roc_prima, mesh){
  etas = numeric()
  etas[1] = ifelse(roc_prima[1] <= 1,
                   ((roc_prima[1] - 1) ^ 2) * mesh[1],
                   ((roc_prima[1] - 1) ^ 2) / roc_prima[1] * roc[1]
  )
  for (i in 2:length(mesh)) {
    etas[i] = ifelse(roc_prima[i] <= 1,
                     ((roc_prima[i] - 1) ^ 2) * (mesh[i] - mesh[i-1]),
                     ((roc_prima[i] - 1) ^ 2) / roc_prima[i] * (roc[i] - roc[i-1])
    )
  }
  eta = sum(etas)
  return(eta)
}



aucs = seq(0.55, 0.99, 0.025)
resultado = matrix(nrow = length(aucs), ncol = 5)
for (i in 1:length(aucs)) {

  
  auc = aucs[i]
  mu = ObtenerMux(auc, sqrt(0.09), sqrt(0.25), 2.5)
  eta = eta_poblacional_II(mu)
  eta_sd = eta / (eta +1)
  eta_trans = log(eta + 1) / (log(eta + 1) + 1)
  
  
  resultado[i,] = c(mu, auc, eta, eta_sd, eta_trans)
  cat(' Media: ', mu ,'\n', 'AUC: ', auc, '\n')
  cat(' eta: ', eta , '\n')
  cat(' eta sd log + 1:', eta_trans, '\n')

}
resultado
df = data.frame(resultado)
colnames(df) = c('media', 'auc', 'eta', 'eta_sd', 'eta_log')
df
write_xlsx(df, 'nueva_estandarizacion_II.xlsx')


# m_plots = c(2.5, 2.84, 3.01, 3.2505)
# for (m in m_plots) {
#   y = rnorm(100000, 2.5, sqrt(0.09))
#   x = rnorm(100000, m, sqrt(0.25))
#   auc = get_auc(y, x)
#   
#   roc_plot(y, x, auc)
# }
