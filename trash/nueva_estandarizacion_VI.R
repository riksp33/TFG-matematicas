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


ObtenerMuMixtura = function(auc_target, Y_pob, var_x_1, var_x_2, offset, tol = 0.01, max_iter = 100){
  lower = mean(Y_pob) - 2
  upper = lower + 6
  iter = 0
  
  while(iter < max_iter){
    bisectriz = (lower + upper) / 2
    iter = iter + 1
    set.seed(1)
    # Muestra de X
    X_1 = rnorm(1e5, bisectriz, sqrt(var_x_1))
    X_2 = rnorm(1e5, bisectriz + offset, sqrt(var_x_2))
    X_pob = HacerMixtura(X_1, X_2, 0.5)
    auc_iter = as.numeric(pROC::auc(response = c(rep(1, 1e5), rep(0, 1e5)), predictor = c(Y_pob, X_pob)))
    cat('AUC iteración ', iter, ': ', auc_iter, '\n')
    
    if(abs(auc_iter - auc_target) < tol){
      return(bisectriz)
    }
    if(auc_iter > auc_target){
      upper = bisectriz
    }
    if(auc_iter < auc_target){
      lower = bisectriz
    }
    
  }
  return(F)
}
HacerMixtura = function(dist_a, dist_b, prob_a) {
  size = length(dist_a) # asumimos tamaños muestrales iguales
  muestra = numeric(size)
  
  for (i in 1:size) {
    if (runif(1) < prob_a) {
      muestra[i] = dist_a[i]
    } 
    else {
      muestra[i] = dist_b[i]
    }
  }
  
  return(muestra)
}
eta_poblacional_VI = function(mu_1, mu_2, sd_1, sd_2, p_mix,mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  p_opp = 1 - p
  
  roc = numeric(mesh_size)
  roc_prima = numeric(mesh_size)
  
  for (i in (1:mesh_size)) {
    point = p_opp[i]
    
    inv = qnorm(point, 0, 1)                                
    numerador = DensidadMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix)        
    denominador = dnorm(inv, 0, 1)
    roc[i] = 1 - DistribucionMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix)
    roc_prima[i] = numerador/denominador
  }
  
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
  y = rnorm(1e5, 0, sqrt(1))
  mu_x = ObtenerMuMixtura(aucs[i], y, 1, 5, offset = 4 )
  
  auc = aucs[i]
  eta = eta_poblacional_VI(mu_x, mu_x + 4, 1, sqrt(5), 0.5)
  eta_sd = eta / (eta +1)
  eta_trans = log(eta + 1) / (log(eta + 1) + 1)
  mu = mu_x
  
  
  resultado[i,] = c(mu, auc, eta, eta_sd, eta_trans)
  cat(' Media: ', mu ,'\n', 'AUC: ', auc, '\n')
  cat(' eta: ', eta , '\n')
  cat(' eta sd log + 1:', eta_trans, '\n')
  
}
resultado
df = data.frame(resultado)
colnames(df) = c('media', 'auc', 'eta', 'eta_sd', 'eta_log')
df
write_xlsx(df, 'nueva_estandarizacion_VI.xlsx')


# m_plots = c(2.5, 2.84, 3.01, 3.2505)
# for (m in m_plots) {
#   y = rnorm(100000, 2.5, sqrt(0.09))
#   x = rnorm(100000, m, sqrt(0.25))
#   auc = get_auc(y, x)
#   
#   roc_plot(y, x, auc)
# }
