directorio = here('kernel','gaussiano','h_paquete', 'Scripts')

scripts = list.files(path = directorio, pattern = "\\.R$", full.names = TRUE)

# Ejecuta cada script
for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  source(script)
}

cat("Todos los scripts han sido ejecutados.")
