# B = 500
# n = 20
# mid = n/2
# controles = rnorm(n, 0, 1)
# casos = rnorm(n, 0.26, 0.3)
# roc_plot(controles, casos)
# etas = numeric(B)
# true = EtaBinormal(controles, casos)
# for (b in 1:B) {
#   muestra = sample(c(controles, casos), replace = T)
#   controles_b = muestra[1:mid]
#   casos_b = muestra[(mid + 1):n]
#   e = EtaBinormal(controles_b, casos_b)
#   etas[b] = e
# }
# 
# hist(etas)
# 
# etas
# p_val = mean(etas >= true)
# cat('El p valor es: ', p_val, '\n')
# 


B = 500
n = 20
mid = n/2
controles = rlnorm(n, 0, 1)
casos = rlnorm(n, 0.28, 0.5)
# roc_plot(controles, casos)
etas = numeric(B)
true = EtaKernel(controles, casos, bc = T, metodo = 'hscv')

for (b in 1:B) {
  muestra = sample(c(controles, casos), replace = T)
  controles_b = muestra[1:mid]
  casos_b = muestra[(mid + 1):n]
  e = EtaKernel(controles_b, casos_b, bc = T, metodo = 'hscv')
  etas[b] = e
}

# Histograma
hist(etas, breaks = 20, col = "steelblue", border = "white",
     main = paste("Distribución de eta bajo H0 (Bootstrap) n = ", n),
     xlab = "Valores de eta", freq = FALSE, xlim = c(0, 1))

# Añade la línea vertical para 'true'
abline(v = true, col = "red", lwd = 2, lty = 2)

# Opcional: Añadir una leyenda para identificar la línea
legend("topright", legend = paste("Valor verdadero =", round(true, 3)), 
       col = "red", lty = 2, lwd = 2)

p_val = mean(etas >= true)
cat('El p valor es: ', p_val, '\n')


