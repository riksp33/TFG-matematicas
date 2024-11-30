library(ggplot2)
library(pROC)
library(gridExtra)
library(grid)

ObtenerRateGamma = function(Y_pob, rate_x, auc_target, tol = 0.01, max_iter = 100){
  lower = 0
  upper = 15
  iter = 0
  
  while(iter < max_iter){
    bisectriz = (lower + upper) / 2
    iter = iter + 1
    set.seed(1)
    # Muestra de X
    X_pob = rgamma(5000, shape = bisectriz, rate = rate_x)
    auc_iter = as.numeric(pROC::auc(response = c(rep(1, 5000), rep(0, 5000)), predictor = c(Y_pob, X_pob)))
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


AUCs <- c(0.6, 0.75, 0.9)
graficos <- list()
# sigmax = 2
# sigmay = 1
# muy = 0


titulo_general <- textGrob(
  "Casos : Gamma(shape, rate = 1/8) | Controles: Gamma(shape = 0.5, rate = 0.5)",
  gp = gpar(fontsize = 16, fontface = "bold")
)
for (auc in AUCs) {

  # Obtener el valor de mu

  
  shapey = 0.5
  ratey = 0.5
  ratex = 1/8
  controles <- rgamma(5000, shape = shapey , rate = ratey)   # Población hardcodeada
  shapex = ObtenerRateGamma(rate_x = ratex, Y_pob = controles, auc_target = auc)
  
  casos <- rgamma(5000, shape = shapex, rate = ratex)  # Población generada con mu calculado
  # Crear data frames para ggplot
  data_densidad <- data.frame(
    valor = c(casos, controles),
    grupo = rep(c("Población 1", "Población 2"), each = 5000)
  )
  
  # Gráfico de densidades solapadas
  plot_densidad <- ggplot(data_densidad, aes(x = valor, fill = grupo)) +
    geom_density(alpha = 0.5) +
    labs(title = bquote("Caso: AUC = "~.(auc)~", shape = "~.(round(shapex, 2)))) +
    theme_minimal() +
    scale_x_continuous(limits = c(0,10)) +  # Fijar escala del eje x
    
    theme(
      legend.position = "none",  # Eliminar la leyenda
      axis.title.x = element_blank(),  # Quitar título del eje x
      axis.title.y = element_blank()   # Quitar título del eje y
    )
  
  # Generar etiquetas para la curva ROC
  etiquetas <- c(rep(1, length(casos)), rep(0, length(controles)))
  valores <- c(casos, controles)
  
  # Calcular curva ROC
  roc_obj <- roc(etiquetas, valores)
  datos_roc <- data.frame(
    tpr = roc_obj$sensitivities,
    fpr = 1 - roc_obj$specificities
  )
  
  # Gráfico de la curva ROC
  plot_roc <- ggplot(datos_roc, aes(x = fpr, y = tpr)) +
    geom_line(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = bquote("Curva ROC: AUC = "~.(round(auc, 2)))) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Eliminar la leyenda
      axis.title.x = element_blank(),  # Quitar título del eje x
      axis.title.y = element_blank()   # Quitar título del eje y
    )
  
  # Guardar gráficos en lista
  graficos <- append(graficos, list(plot_densidad, plot_roc))
}

# Combinar gráficos en una cuadrícula (3 filas x 2 columnas)
# Combinar gráficos en una cuadrícula con título general
grid.arrange(
  titulo_general,  # Título en la parte superior
  arrangeGrob(grobs = graficos, ncol = 2),  # Gráficos en 3x2
  nrow = 2,
  heights = c(0.1, 1)  # Ajustar la altura del título y la cuadrícula
)

