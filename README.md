# Trabajo Fin de Grado en Matemáticas. Ricardo de la Azuela Sánchez


## Paquetes
Para la gestión de archivos json, paths relativos y selección del parámetro de suavizado h se han utilizado los paquetes "jsonlite", "here" y "ks" respectivamente. El script "package_manager.R" se encarga de la gestión de estos.
**Es esencial ejecutar este archivo antes lanzar la primera simulación.**

## Estructura de los archivos

Los archivos están organizados por carpetas, una para cada método de estimación. Dentro de la carpeta de cada método se encuentran 3 elementos: 
* Scripts: contiene los scripts necesarios para realizar las simulaciones
* jsons: guarda los archivos json con los resultados de cada simulación
* Simulate.R: script ejecutable para lanzar todas las simulaciones del escenario considerado

En la raiz del repo se encuentra también el script "funciones_auxiliares.R" el cual se encarga de definir las funciones necesarias para estimar eta, generar mixturas...

También existe el script de python "table_generator.py" el cual ayuda a la traducción de los resultados desde el formato .json a tablas de LaTex.

El archivo "Simulate_all.R" agrupa todos los scripts de simulación, lanzando todos los escenarios al ser ejecutado.

  
