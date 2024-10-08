directorio = "/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/parametrico/Scripts"
setwd('/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/parametrico/jsons')

scripts = list.files(path = directorio, pattern = "\\.R$", full.names = TRUE)

for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  source(script)
}

cat("Todos los scripts han sido ejecutados.")
