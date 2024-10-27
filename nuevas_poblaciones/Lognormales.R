eta_poblacional_IV_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 0, 1)
  numerador = dlnorm(inv, mux, 0.5)
  denominador = dlnorm(inv, 0, 1)
  
  roc = 1 - plnorm(inv, mux, 0.5 )
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_V_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 0, 1)
  numerador = dlnorm(inv, mux, 3/2)
  denominador = dlnorm(inv, 0, 1)
  
  roc = 1 - plnorm(inv, mux, 3/2 )
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_VI_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 0, 1)
  numerador = dlnorm(inv, mux, 0.2)
  denominador = dlnorm(inv, 0, 1)
  
  roc = 1 - plnorm(inv, mux, 0.2 )
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_VII_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 0, 1)
  numerador = dlnorm(inv, mux, 2)
  denominador = dlnorm(inv, 0, 1)
  
  roc = 1 - plnorm(inv, mux, 2 )
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_VIII_n = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 0, 1)
  numerador = dlnorm(inv, mux, 0.5)
  denominador = dlnorm(inv, 0, 1)
  
  roc = 1 - plnorm(inv, mux, 0.5 )
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

