eta_poblacional_II = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 2.5, sqrt(0.09))
  numerador = dnorm(inv, mux, sqrt(0.25))
  denominador = dnorm(inv, 2.5, sqrt(0.09))
  
  roc = 1 - pnorm(inv, mux, sqrt(0.25))
  plot(roc, type = 'l')
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



m = seq(2.55, 5, 0.05)
resultado = matrix(nrow = length(m), ncol = 5)
for (i in 1:length(m)) {
  y = rnorm(1000, 2.5, sqrt(0.09))
  x = rnorm(1000, m[i], sqrt(0.25))
  
  auc = get_auc(y, x)
  eta = eta_poblacional_I(mu, 1000)
  eta_sd = eta / (eta +1)
  eta_trans = log(eta + 1) / (log(eta + 1) + 1)
  mu = m[i]
  
  resultado[i,] = c(mu, auc, eta, eta_sd, eta_trans)
  cat(' Media: ', mu ,'\n', 'AUC: ', auc, '\n')
  cat(' eta: ', eta , '\n')
  cat(' eta sd log + 1:', eta_trans, '\n')

}
resultado
df = data.frame(resultado)
colnames(df) = c('media', 'auc', 'eta', 'eta_sd', 'eta_log')
df
