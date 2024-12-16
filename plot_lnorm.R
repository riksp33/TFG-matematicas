# Instalar paquetes necesarios si no están ya instalados
if (!require(pROC)) install.packages("pROC")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork")
if (!require(dplyr)) install.packages("dplyr")

# Cargar librerías
library(pROC)
library(ggplot2)
library(patchwork)
library(dplyr)

# Generar datos para población fija de sanos (N(0,1)) y tres poblaciones de enfermos
set.seed(123)
n <- 10000# Instalar paquetes necesarios si no están ya instalados
if (!require(pROC)) install.packages("pROC")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(patchwork)) install.packages("patchwork")
if (!require(dplyr)) install.packages("dplyr")

# Cargar librerías
library(pROC)
library(ggplot2)
library(patchwork)
library(dplyr)

# Rango común para graficar las distribuciones analíticas
x_range <- seq(0, 6, length.out = 1000)

# Definir las densidades de las distribuciones
dens_sanos <- dlnorm(x_range, 0, 1)
dens_enfermos1 <- dlnorm(x_range, 0.28, 0.5)
dens_enfermos2 <- dlnorm(x_range, 0.46, 3/2)
dens_enfermos3 <- dlnorm(x_range, 0.26, 0.2)
dens_enfermos4 <- dlnorm(x_range, 0.57, 2)

# Crear dataframes para ggplot de las densidades
df_dens1 <- data.frame(Value = x_range,
                       Density = dens_sanos,
                       Group = "Sano",
                       Case = "Caso 1") %>%
  rbind(data.frame(Value = x_range,
                   Density = dens_enfermos1,
                   Group = "Enfermo",
                   Case = "Caso 1"))

df_dens2 <- data.frame(Value = x_range,
                       Density = dens_sanos,
                       Group = "Sano",
                       Case = "Caso 2") %>%
  rbind(data.frame(Value = x_range,
                   Density = dens_enfermos2,
                   Group = "Enfermo",
                   Case = "Caso 2"))

df_dens3 <- data.frame(Value = x_range,
                       Density = dens_sanos,
                       Group = "Sano",
                       Case = "Caso 3") %>%
  rbind(data.frame(Value = x_range,
                   Density = dens_enfermos3,
                   Group = "Enfermo",
                   Case = "Caso 3"))

df_dens4 <- data.frame(Value = x_range,
                       Density = dens_sanos,
                       Group = "Sano",
                       Case = "Caso 4") %>%
  rbind(data.frame(Value = x_range,
                   Density = dens_enfermos4,
                   Group = "Enfermo",
                   Case = "Caso 4"))

# Simulaciones para curvas ROC
set.seed(123)
n <- 10000

# Sanos: N(0,1)
sanos <- rlnorm(n, 0, 1)

# Tres poblaciones de enfermos
enfermos1 <- rlnorm(n, 0.28, 0.5)  # Caso 1
enfermos2 <- rlnorm(n, 0.44,  1.4)  # Caso 2
enfermos3 <- rlnorm(n, 0.26, 0.3)  # Caso 3
enfermos4 <- rlnorm(n,  0.57, 0.3)  # Caso 3

# Etiquetas para las curvas ROC
etiquetas <- c(rep(0, n), rep(1, n))

# Calcular curvas ROC
roc1 <- roc(etiquetas, c(sanos, enfermos1))
roc2 <- roc(etiquetas, c(sanos, enfermos2))
roc3 <- roc(etiquetas, c(sanos, enfermos3))
roc4 <- roc(etiquetas, c(sanos, enfermos4))

# Crear dataframes para las curvas ROC
roc_data1 <- data.frame(Specificity = 1 - roc1$specificities, Sensitivity = roc1$sensitivities, Case = "Caso 1")
roc_data2 <- data.frame(Specificity = 1 - roc2$specificities, Sensitivity = roc2$sensitivities, Case = "Caso 2")
roc_data3 <- data.frame(Specificity = 1 - roc3$specificities, Sensitivity = roc3$sensitivities, Case = "Caso 3")
roc_data4 <- data.frame(Specificity = 1 - roc4$specificities, Sensitivity = roc4$sensitivities, Case = "Caso 4")

# Crear gráficos de distribuciones analíticas
plot_distribution1 <- ggplot(df_dens1, aes(x = Value, y = Density, fill = Group)) +
  geom_area(position = "identity", alpha = 0.5, color = "gray80", size = 0.3) +  # Perfilado suave
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),  # Líneas del grid más claras
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("LN(0.28, 0.5)")

plot_distribution2 <- ggplot(df_dens2, aes(x = Value, y = Density, fill = Group)) +
  geom_area(position = "identity", alpha = 0.5, color = "gray80", size = 0.3) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("LN(0.46, 3/2)")

plot_distribution3 <- ggplot(df_dens3, aes(x = Value, y = Density, fill = Group)) +
  geom_area(position = "identity", alpha = 0.5, color = "gray80", size = 0.3) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("LN(0.26, 0.2)")

plot_distribution4 <- ggplot(df_dens4, aes(x = Value, y = Density, fill = Group)) +
  geom_area(position = "identity", alpha = 0.5, color = "gray80", size = 0.3) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("LN(0.57, 2)")

# Crear gráficos de curvas ROC
plot_roc1 <- ggplot(roc_data1, aes(x = Specificity, y = Sensitivity)) +
  geom_line(size = 1, color = 'steelblue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("")

plot_roc2 <- ggplot(roc_data2, aes(x = Specificity, y = Sensitivity)) +
  geom_line(size = 1, color = 'steelblue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("")

plot_roc3 <- ggplot(roc_data3, aes(x = Specificity, y = Sensitivity)) +
  geom_line(size = 1, color = 'steelblue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("")

plot_roc4 <- ggplot(roc_data4, aes(x = Specificity, y = Sensitivity)) +
  geom_line(size = 1, color = 'steelblue') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray95", size = 0.2),
    panel.grid.minor = element_line(color = "gray98", size = 0.1)
  ) +
  ggtitle("")

# Combinar gráficos por filas (Distribución + ROC)
row1 <- plot_distribution1 | plot_roc1
row2 <- plot_distribution2 | plot_roc2
row3 <- plot_distribution3 | plot_roc3
row4 <- plot_distribution4 | plot_roc4

# Combinar todas las filas verticalmente
combined_plot <- row1 / row2 / row3 / row4

# Mostrar el gráfico combinado
print(combined_plot)

