library(jsonlite)
library(ks)

l = 0.000001
u = 0.999999
m = 100000

################################################################################
# FUNCIONES GENERALES

# Saca el sesgo dado un vector de estimaciones y un valor real
GetBias = function(estimaciones , valor_real_estimador){
  bias = mean(estimaciones) - valor_real_estimador
  return(bias)
}

# Saca el rmse dado un vector de estimaciones y un valor real
GetRMSE = function(estimaciones , valor_real_estimador){
  
  RMSE = sqrt(mean((estimaciones - valor_real_estimador)^2))
  return(RMSE)
}

# Usa (6) de Faraggi para sacar el valor de mux
ObtenerMux = function(AUC , sigmax , sigmay , muy){
  mux = sqrt(sigmay^2 + sigmax^2) * qnorm(AUC) + muy
  return(mux)
}

estandarizacion = function(x) {
  return(log(x +1)/ (1 + log(x+1)) )
}

################################################################################
# FUNCIONES PARA GENERAR LOS ETAS REALES
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
  return(estandarizacion(eta)
 )
}

eta_poblacional_I = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qnorm(1-p, 2.5, sqrt(0.25))
  numerador = dnorm(inv, mux, sqrt(0.25))
  denominador = dnorm(inv, 2.5, sqrt(0.25))
  
  roc = 1 - pnorm(inv, mux, sqrt(0.25))
  roc_prima = numerador / denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
  
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

eta_poblacional_III = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)

  inv = qnorm(1-p, 2.5, sqrt(0.09))^3
  numerador = dnorm(inv^(1/3), mux, sqrt(0.25)) * (1/3) * (inv^(-2/3))
  denominador = dnorm(inv^(1/3), 2.5, sqrt(0.09))* (1/3) * (inv^(-2/3))
  
  roc = 1 - pnorm(inv ^ (1/3), mux, sqrt(0.25))
  roc_prima = numerador/denominador

  return(eta_pob_sd_stable(roc, roc_prima, p))
  
}

eta_poblacional_IV = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  
  inv = qlnorm(1-p, 2.5, sqrt(0.25))
  numerador = dlnorm(inv, mux, sqrt(0.09))
  denominador = dlnorm(inv, 2.5, sqrt(0.25))
  
  roc = 1 - plnorm(inv, mux, sqrt(0.09) )
  roc_prima = numerador/denominador

  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_V_05 = function(mesh_size = m){
  p = seq(l, u, length.out = mesh_size)

  inv = qgamma(1-p, shape = 2, rate = 0.5)
  numerador = dgamma(inv, shape = 2, rate = 0.12)
  denominador = dgamma(inv, shape = 2, rate = 0.5)
  
  roc = 1 - pgamma(inv, shape = 2, rate = 0.12)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_V_1 = function(mux, mesh_size = m){
  p = seq(l, u, length.out = mesh_size)

  inv = qgamma(1-p, shape = 2, rate = 1)
  numerador = dgamma(inv, shape = 2, rate = 0.24)
  denominador = dgamma(inv, shape = 2, rate = 1)
  
  roc = 1 - pgamma(inv, shape = 2, rate = 0.24)
  roc_prima = numerador/denominador
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
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

eta_poblacional_VII = function(mu_1, mu_2, sd_1, sd_2, p_mix,mesh_size = m){
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

eta_poblacional_VIII = function(mu_1, mu_2, sd_1, sd_2, p_mix,mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  p_opp = 1 - p
  
  roc = numeric(mesh_size)
  roc_prima = numeric(mesh_size)
  
  for (i in (1:mesh_size)) {
    point = p_opp[i]
  
    inv = CuantilMixtura(point, 0, 3, 1, 1, 0.5)                               
    numerador = DensidadMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix)        
    denominador = DensidadMixtura(inv, 0, 3, 1, 1, 0.5)
    roc[i] = 1 - DistribucionMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix)
    roc_prima[i] = numerador/denominador
  }
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
}

eta_poblacional_IX = function(mu_1, mu_2, sd_1, sd_2, p_mix,mesh_size = m){
  p = seq(l, u, length.out = mesh_size)
  p_opp = 1 - p
  
  roc = numeric(mesh_size)
  roc_prima = numeric(mesh_size)
  
  for (i in (1:mesh_size)) {
    point = p_opp[i]
  
    inv = CuantilMixtura(point, 0, 3, 1, sqrt(1.5), 0.5)                               
    numerador = DensidadMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix)        
    denominador = DensidadMixtura(inv, 0, 3, 1, sqrt(1.5), 0.5)
    roc[i] = 1 - DistribucionMixtura(inv, mu_1, mu_2, sd_1, sd_2, p_mix) 
    roc_prima[i] = numerador/denominador
  }
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
  
}

################################################################################
# FUNCIONES PARA MIXTURAS

# Genera una mixtura dadas dos muestras y el valor p
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

# Obtiene el valor de la desviación de una mixtura dadas las varianzas y p
GetStdMixtura = function(var_a, var_b, p_a){
  var = (p_a^2)*var_a + ((1-p_a)^2)*var_b
  return(sqrt(var))
}

# Obtiene la densidad de una mixtura de normales dados sus parametros
DensidadMixtura = function(punto, mu_1, mu_2, sd_1, sd_2, p){
  value = p*dnorm(punto, mu_1, sd_1) + (1-p)*dnorm(punto, mu_2, sd_2)
  return(value)
}

# Obtiene la distribucion de una mixtura dados sus parametros
DistribucionMixtura = function(punto, mu_1, mu_2, sd_1, sd_2, p){
  value = p * pnorm(punto, mu_1, sd_1) + (1-p)*pnorm(punto, mu_2, sd_2)
  return(value)
}

# Obtiene el cuantil para una densidad normal de mixtura dados sus parametros
# Usa el método de la bisectriz
CuantilMixtura = function(punto, mu_1, mu_2, sd_1,
                          sd_2, p_mixtura, tol = 1e-8, max_iter = 100){
  lower = -10
  upper = 10
  iter = 0
  
  while (iter <= max_iter) {
    iter = iter + 1
    bisectriz = (lower + upper) / 2
    valor = DistribucionMixtura(bisectriz, mu_1, mu_2, sd_1, sd_2, p_mixtura)
    
    # Caso dentro de tolerancia
    if (abs(punto - valor) < tol) {
      return(bisectriz)
    }
    
    # Caso a la izquierda
    if(punto < valor){
      upper = bisectriz
    }
    
    if(punto > valor){
      lower = bisectriz
    }
    
  }
  
  print('Max iter superado')
  return(F)
}

ObtenerMuMixtura = function(auc_target, Y_pob, var_x_1, var_x_2, offset, tol = 0.01, max_iter = 100){
  lower = mean(Y_pob) - 2
  upper = lower + 4
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

################################################################################
# FUNCIONES PARA ESTIMACIÓN NO PARAMETRICA
EtaEmpirica = function(control , casos){
  
  nd = length(casos)
  n_d_bar = length(control)
  
  control = sort(control)
  
  aux = numeric()
  
  aux[1] = sum(casos <= control[1])
  
  aux[n_d_bar + 1] = sum(casos > control[n_d_bar])
  
  anterior = sum(casos <= control[1])
  
  for (k in 2:n_d_bar) {
    actual = sum(casos <= control[k])
    
    aux[k] = actual - anterior
    
    anterior = actual
  }
  
  eta = sum((aux/n_d_bar - 1/(n_d_bar + 1))^2)
  
  return(estandarizacion(eta)
)
}

################################################################################
# FUNCIONES PARA ESTIMACION PARAMETRICA

HacerBoxCox = function(xo, yo, P =F){
  number_negative_obs = sum(xo <= 0) + sum(yo <= 0)
  if (number_negative_obs > 0 ) {
    constant = -min(min(xo) , min(yo)) + 5e-4
    xo = xo + constant
    yo = yo + constant
  }
  likbox=function(h,data,n){
    m=length(data)-n
    x=data[1:n]
    y=data[(n+1):length(data)]
    if (abs(h)<1e-5){
      xh=log(x)
      yh=log(y)
    } else {
      xh=((x^h)-1)/h
      yh=((y^h)-1)/h
    }
    oout=-n/2*log(sum((xh-sum(xh)/n)^2)/n)-m/2*log(sum((yh-sum(yh)/m)^2)/m) +(h-1)*(sum(log(x))+sum(log(y)))
    return(-oout)
  }
  
  h_ini=-0.6
  hhat=optim(h_ini,likbox,data=c(xo,yo),n=length(xo),method="BFGS")$par
  if (abs(hhat)<1e-5){
    muestra1=log(xo)
    muestra2=log(yo)
  } else {
    muestra1=((xo^hhat)-1)/hhat
    muestra2=((yo^hhat)-1)/hhat
  }
  
  if (P) {
    cat('El lambda de la transformada es: ', hhat, '\n')
  }
  df = data.frame(muestra1,muestra2)
  return(df)
}


EtaBinormal=function(controles,casos, bc = F, p=seq(0.00001,0.99999,length.out=10000)){
  
  if (bc) {
    df_trans = HacerBoxCox(controles, casos)
    controles = df_trans$muestra1
    casos = df_trans$muestra2
  }
  
  
  mux=mean(controles)
  muy=mean(casos)
  sigmax=sd(controles)
  sigmay=sd(casos)
  ro=sigmax/sigmay
  delta=(muy-mux)/sigmay
  ROC=1-pnorm(qnorm(1-p,mux,sigmax),mux+delta*sigmax/ro,sigmax/ro)
  ROCprima=(ro*exp(-0.5*(delta+ro*qnorm(p))^2))/exp(-0.5*(qnorm(p))^2)
  etaaux=numeric(10000)
  etaaux[1]=ifelse(ROCprima[1]<=1,(ROCprima[1]-1)^2*p[1],(ROCprima[1]-1)^2/ROCprima[1]*ROC[1])
  for (k in 2:length(p)){
    etaaux[k]=ifelse(ROCprima[k]<=1,(ROCprima[k]-1)^2*(p[k]-p[k-1]),(ROCprima[k]-1)^2/(ROCprima[k])*(ROC[k]-ROC[k-1]))
  }
  eta=sum(etaaux)
  return(estandarizacion(eta)
)
}

################################################################################
# FUNCIONES PARA ESTIMACION KERNEL

# Obtiene la estimación de la densidad por Kernel
DensidadKernel=function(datos,puntos,h){
  ndatos=length(datos)
  npuntos=length(puntos)
  matk=dnorm((puntos%*%t(rep(1,ndatos))-t(datos%*%t(rep(1,npuntos))))/h)
  as.vector((matk%*%rep(1,ndatos))/(ndatos*h))
}

# Obtiene la estimación de la distribución por Kernel
DistKernel=function(datos,puntos,h){
  ndatos=length(datos)
  npuntos=length(puntos)
  matk=pnorm((puntos%*%t(rep(1,ndatos))-t(datos%*%t(rep(1,npuntos))))/h)
  as.vector((matk%*%rep(1,ndatos))/(ndatos))
}

# Evalua sobre la estimación Kernel
Evaluate = function(punto, funcion, mesh){
  f_sorted = sort(funcion)
  l = length(mesh)
  posicion = sum((mesh < punto))
  r_index = 1
  
  if (posicion == l) {
    r_index = l -1
  }
  if ((posicion < l) & (posicion > 1)) {
    r_index = posicion
  }
  
  value = mean(c(funcion[r_index] ,funcion[r_index + 1]))
  return(value)
}

# Halla el cuantil de la estimación Kernel
Inverse = function(punto, funcion, mesh){
  f_sorted = sort(funcion)
  l = length(mesh)
  posicion = sum(funcion < punto)
  r_index = 1
  
  if (posicion == l) {
    r_index = l -1
  }
  if ((posicion < l) & (posicion > 1)) {
    r_index = posicion
  }
  
  value = mean(c(mesh[r_index] ,mesh[r_index + 1]))
  return(value)
}

# Obtiene la estimacion de Eta a través de kernel
EtaKernel = function(muestra_sanos, muestra_enfermos, metodo, mesh_size = 1000){
  if (metodo == 'optimo') {
    h_sanos= 1.06*sd(muestra_sanos)*length(muestra_sanos)^(-1/5)
    h_enfermos= 1.06*sd(muestra_enfermos)*length(muestra_enfermos)^(-1/5)
  }
  if (metodo == 'hscv'){
    h_sanos = hscv(muestra_sanos)
    h_enfermos = hscv((muestra_enfermos)) 
  }
  
  
  sorted_sanos = sort(muestra_sanos)
  sorted_enfermos = sort(muestra_enfermos)
  
  mesh = seq(min(c(muestra_sanos,muestra_enfermos)),
             max(c(muestra_sanos,muestra_enfermos)),
             length.out = mesh_size)
  
  estimated_dist_sanos = DistKernel(sorted_sanos,mesh,h_sanos)
  estimated_dist_enfermos = DistKernel(sorted_enfermos,mesh,h_enfermos)
  estimated_dens_sanos = DensidadKernel(sorted_sanos, mesh,h_sanos)
  estimated_dens_enfermos = DensidadKernel(sorted_enfermos, mesh,h_enfermos)
  
  p = seq(0.0001,0.999, length.out = mesh_size)
  p_opp = 1-p
  
  # etas = numeric(mesh_size)
  roc = numeric(mesh_size)
  roc_prima = numeric(mesh_size)
  
  for (i in (1:mesh_size)) {
    point = p_opp[i]
    
    inv = Inverse(point, estimated_dist_sanos, mesh)
    numerador = Evaluate(inv, estimated_dens_enfermos, mesh)
    denominador = Evaluate(inv, estimated_dens_sanos, mesh)
    
    roc[i] = 1 - Evaluate(inv, estimated_dist_enfermos, mesh)
    roc_prima[i] = numerador / denominador
    
    # eta = numerador/denominador
    # etas[i] = eta
    # 
  }
  
  return(eta_pob_sd_stable(roc, roc_prima, p))
  
  # integrando = (etas -1)^2
  # base = mesh[2]-mesh[1]
  # 
  # integral = 0
  # for (eta in integrando) {
  #   integral = integral + (eta*base)
  #   
  # }
  # return(estandarizacion(eta)
 
}

##################################################################
# FUNCIÓN PARA ENCONTRAR EL RATE ADECUADO PARA EL VALOR DE AUC

ObtenerRateGamma = function(Y_pob, rate_x, auc_target, tol = 0.01, max_iter = 100){
  lower = 0
  upper = 15
  iter = 0
  
  while(iter < max_iter){
    bisectriz = (lower + upper) / 2
    iter = iter + 1
    set.seed(1)
    # Muestra de X
    X_pob = rgamma(1e5, shape = bisectriz, rate = rate_x)
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

