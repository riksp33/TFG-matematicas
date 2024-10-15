# Método para nuevo cálculo de eta
# Estoy usando datos de una distribución concreta para hacer la prueba

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


# Parámetros de cálculo
p = seq(0.0000001, 0.9999999, length.out = 1000)

sacar_curvas = function(p, mux){
  
  p_opp = 1 - p
  
  # Saco la función ROC' calculada de forma discreta
  point = p_opp
  inv = qnorm(point, 2.5, sqrt(0.25))
  numerador = dnorm(inv, mux, sqrt(0.25))
  denominador = dnorm(inv, 2.5, sqrt(0.25))
  roc_prima = numerador/denominador
  
  # plot(roc_prima) # Compruebo la forma de la cruva
  
  # Saco la función ROC calculada de forma discreta
  inversa = qnorm(p_opp, 2.5, sqrt(0.25))
  roc = 1 - pnorm(inversa, mux, sqrt(0.25))
  
  # plot(roc) # Compruebo la forma de la curva
  
  return(list(
    roc = roc,
    roc_prima = roc_prima
  ))
  
}


# Hago la integral entre 0 y 1
new_eta_sd = function(roc, roc_prima, p){
  
  integrando = numeric(length = length(p))
  for (i in 1:length(p)) {
    roc_prima_sin_transformar = Evaluate(p[i], roc_prima, p)
    
    # Caso 1: no necesito hacer transformación porque la derivada es < 1
    if ( roc_prima_sin_transformar < 1) {
      integrando[i] = ((roc_prima_sin_transformar - 1) ^ 2)* p[1]
    }
    
    # Caso 2: la derivada es mayor que 1 y hago la transformación
    else{
      
      # Evaluamos la inversa de la curva roc en cada punto de la malla
      inversa_roc = Inverse(p[i], roc, p)
      
      # Evaluamos cada punto de la inversa en la ROC'
      roc_prima_de_inversa = Evaluate(inversa_roc, roc_prima, p)
      

      
      # Calculamos el valor del integrando ya dividiendo también
      base = ifelse(i == 1, roc[1], roc[i] - roc[i-1])
      integrando[i] = (((roc_prima_de_inversa - 1)^2) / roc_prima_de_inversa)*base
  
      
    }
    
  }
  
  # Valor de eta
  eta = sum(integrando)
  
  # Valor de eta estandarizada
  eta_sd = eta/(eta +1)
  
  return(eta_sd)
}


# curvas = sacar_curvas(p, 8)
# new_eta_sd(curvas$roc, curvas$roc_prima, p)

d_size = 1000
deltas = seq(0.05, 2, length.out = d_size )
etas = numeric(d_size)
for (i in 1:d_size) {
  curvas = sacar_curvas(p, 2.5 + deltas[i])
  etas[i] = new_eta_sd(curvas$roc, curvas$roc_prima, p)
}
plot(deltas, etas)

# 
# # Pruebas para ver que la inversa la calculo correctamente
# mesh_prueba = seq(0.01, 0.99, 0.05)
# 
# for (punto in mesh_prueba) {
#   
# roc_prueba = 1 - pnorm(qnorm(1 - punto, 2.5, sqrt(0.25)), mux, sqrt(0.25))
# inversa_roc_prueba = Inverse(roc_prueba, roc, p)
# 
# stopifnot(abs(inversa_roc_prueba - punto) < 0.01)
# }
# print('Todos pasan')
# 
# 

max(etas)


etas[deltas > 4]
