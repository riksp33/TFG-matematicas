eta_poblacional_IX_n = function(rate, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qgamma(1-p, shape = 0.5, rate = 0.5)
  numerador = dgamma(inv, shape = 2, rate = rate)
  denominador = dgamma(inv, shape = 0.5, rate = 0.5)
  
  roc = 1 - pgamma(inv, shape = 2, rate = rate)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_X_n = function(rate, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qgamma(1-p, shape = 0.5, rate = 0.5)
  numerador = dgamma(inv, shape = 4, rate = rate)
  denominador = dgamma(inv, shape = 0.5, rate = 0.5)
  
  roc = 1 - pgamma(inv, shape = 4, rate = rate)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_XI_n = function(rate, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qgamma(1-p, shape = 0.5, rate = 0.5)
  numerador = dgamma(inv, shape = 4.3, rate = rate)
  denominador = dgamma(inv, shape = 0.5, rate = 0.5)
  
  roc = 1 - pgamma(inv, shape = 4.3, rate = rate)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_XII_n = function(rate, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qgamma(1-p, shape = 0.5, rate = 0.5)
  numerador = dgamma(inv, shape = 1/8, rate = rate)
  denominador = dgamma(inv, shape = 0.5, rate = 0.5)
  
  roc = 1 - pgamma(inv, shape = 1/8, rate = rate)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}