# Función original para calcular eta
eta_original <- function(mux, p) {
  p_opp = 1 - p
  inv = qnorm(p_opp, 2.5, sqrt(0.25))
  numerador = dnorm(inv, mux, sqrt(0.25))
  denominador = dnorm(inv, 2.5, sqrt(0.25))
  etas = numerador / denominador
  
  # Hacemos la integral numérica para calcular eta
  integrando = (etas - 1)^2
  base = p[2] - p[1]
  integral = sum(integrando) * base
  
  # Valor de eta
  return(integral / (integral + 1))
}

# Definir los diferentes tamaños de malla que queremos comparar
mesh_sizes <- c(1000, 10000, 50000, 100000)

# Valores de delta
n_len = 50
deltas = seq(0, 9, length.out = n_len)

# Inicializar matrices para almacenar resultados de Alba, Rik y método original
alba_results <- matrix(0, nrow = n_len, ncol = length(mesh_sizes))
rik_results <- matrix(0, nrow = n_len, ncol = length(mesh_sizes))
original_results <- matrix(0, nrow = n_len, ncol = length(mesh_sizes))

# Establecer los colores para las diferentes mallas
colors <- rainbow(length(mesh_sizes))

# Configurar el espacio para gráficos
par(mfrow = c(2, 2))  # Para crear una cuadrícula de 2x2 gráficos

# Bucle sobre los tamaños de malla
for (j in 1:length(mesh_sizes)) {
  mesh_size <- mesh_sizes[j]
  
  # Crear la malla de probabilidades 'p'
  p = seq(1/mesh_size, 1 - (1/mesh_size), length.out = mesh_size)
  
  # Bucle sobre los valores de delta
  for (i in 1:n_len) {
    delta = deltas[i]
    
    # Alba
    x = rnorm(100000, 2.5, sqrt(0.25))
    y = rnorm(100000, 2.5 + delta, sqrt(0.25))
    alba_results[i, j] = eta_sd_version_alba(x, y, p)
    
    # Rik
    curvas = sacar_curvas(p, 2.5 + delta)
    rik_results[i, j] = new_eta_sd(curvas$roc, curvas$roc_prima, p)
    
    # Método original
    original_results[i, j] = eta_original(2.5 + delta, p)
  }
  
  # Graficar los resultados para el tamaño de malla actual
  plot(deltas, alba_results[, j], type = 'l', col = 'steelblue', ylab = 'eta_sd',
       xlab = 'Delta', ylim = range(c(alba_results, rik_results, original_results)),
       main = paste('Comparación con malla de tamaño', mesh_size))
  
  # Añadir la línea para Rik
  lines(deltas, rik_results[, j], col = 'green')
  
  # Añadir la línea para el método original
  lines(deltas, original_results[, j], col = 'red')
  
  # Añadir leyenda
  legend('topright',
         legend = c('Función Alba', 'Función propia (Rik)', 'Método original'),
         col = c('steelblue', 'green', 'red'),
         lwd = 2)
}

# Añadir un título general al conjunto de gráficos
# mtext("Comparación de eta_sd para diferentes tamaños de malla", side = 3, line = -2, outer = TRUE)
