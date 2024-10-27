
eta_pob_I_n = function (mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 0, 1)
  numerador = dnorm(inv, mux, 1)
  denominador = dnorm(inv, 0, 1)
  
  roc = 1 - pnorm(inv, mux, 1)
  roc_prima = numerador / denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_pob_II_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 0, 1)
  numerador = dnorm(inv, mux, 1.4)
  denominador = dnorm(inv, 0, 1)
  
  roc = 1 - pnorm(inv, mux, 1.4)
  roc_prima = numerador / denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_pob_III_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 0, 1)
  numerador = dnorm(inv, mux, 0.3)
  denominador = dnorm(inv, 0, 1)
  
  roc = 1 - pnorm(inv, mux, 0.3)
  roc_prima = numerador / denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}
