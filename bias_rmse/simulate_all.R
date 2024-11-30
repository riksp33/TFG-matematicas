# Limpia el entorno antes de ejecutar nada
rm(list = ls())

# Define las funciones auxiliares
dir = here('funciones_auxiliares.R')
source(dir)
# Define las funciones para cada población

dir = here('bias_rmse', 'Etas_poblacionales_nuevas.R')
source(dir)

# Simula todos los casos

directorio = here('bias_rmse', 'sim_scripts')
scripts = list.files(path = directorio, pattern = "\\.R$", full.names = TRUE)

for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  source(script)
}

cat("Todos los scripts han sido ejecutados.")
