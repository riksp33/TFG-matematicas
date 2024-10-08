directorio = "/Users/Riki/Desktop/ucm/TFGs/Mates/codigo/codigo_final/kernel/gaussiano/h_paquete/Scripts"
setwd('/Users/Riki/Desktop/ucm/TFGs/Mates/codigo/codigo_final/kernel/gaussiano/h_paquete/jsons')

# Lista todos los archivos .R en el directorio
scripts = list.files(path = directorio, pattern = "\\.R$", full.names = TRUE)

# Ejecuta cada script
for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  source(script)
}

cat("Todos los scripts han sido ejecutados.")
