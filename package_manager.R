# Instala las librerias necesarias para correr todos los scripts de forma correcta

depencencies = c('ks', 'jsonlite', 'here')

install_if_missing = function(packages) {
  missing_packages = packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    cat("Instalando los paquetes que faltan...\n")
    install.packages(missing_packages)
  } else {
    cat("Todos los paquetes están instalados.\n")
  }
}

install_if_missing(depencencies)

lapply(depencencies, library, character.only = TRUE)

cat("Todos los paquetes han sido cargados correctamente.\n")