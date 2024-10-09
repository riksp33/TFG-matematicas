directorio = here('no_parametrico', 'Scripts')
scripts = list.files(path = directorio, pattern = "\\.R$", full.names = TRUE)

for (script in scripts) {
  cat("Ejecutando:", script, "\n")
  source(script)
}

cat("Todos los scripts han sido ejecutados.")
