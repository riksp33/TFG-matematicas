# Definir los diferentes tamaños de malla que queremos comparar
mesh_sizes <- c(1000, 5000, 10000)  # Puedes ajustar los tamaños de malla si lo necesitas

# Valores de delta para representar la variación
n_len = 50
deltas = seq(0, 9, length.out = n_len)

# Inicializar una matriz para almacenar los resultados de Alba
alba_results <- matrix(0, nrow = n_len, ncol = length(mesh_sizes))

# Establecer los colores para las diferentes mallas
colors <- rainbow(length(mesh_sizes))

# Preparar el gráfico vacío
plot(1, type = "n", xlim = range(deltas), ylim = c(0, 1), 
     xlab = "Delta", ylab = "eta_sd", 
     main = "Evolución de eta_sd para diferentes tamaños de malla")

# Bucle sobre los tamaños de malla
for (j in 1:length(mesh_sizes)) {
  mesh_size <- mesh_sizes[j]
  
  # Crear la malla de probabilidades 'p' con el tamaño de malla actual
  p = seq(0.00001, 0.99999, length.out = mesh_size)
  
  # Bucle sobre los valores de delta
  for (i in 1:n_len) {
    delta = deltas[i]
    
    # Alba: calcular eta usando la versión de Alba para cada valor de delta
    x = rnorm(100000, 2.5, sqrt(0.25))
    y = rnorm(100000, 2.5 + delta, sqrt(0.25))
    alba_results[i, j] = eta_sd_version_alba(x, y, p)
  }
  
  # Añadir la línea correspondiente al tamaño de malla actual
  lines(deltas, alba_results[, j], col = colors[j], lwd = 2, type = "l", pch = 19)
}

# Añadir leyenda
legend("topright", legend = paste("Malla =", mesh_sizes), col = colors, lwd = 2, pch = 19)
