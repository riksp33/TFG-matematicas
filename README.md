# Trabajo Fin de Grado en Matemáticas: Ricardo de la Azuela Sánchez

## Resumen

El objetivo del código de este repositorio es la realización de un estudio de simulación para el estadístico eta presentado en **"NON PARAMETRIC ROC SUMMARY STATISTICS"** por MC. Pardo y AM. Franco. junto a otros métodos de estimación. En el análisis se han comprobado resultados de sesgo y RMSE así como potencias y niveles del test estadístico que toma como H0 que el clasificador es no informativo.

## Sesgo y RMSE

Los cáclulos de sesgo y RMSE se realizan en los scripts de la carpeta `bias_rmse`. Se han realizado un tamaño de montecarlo de 1000, para poblaciones de tamaño 20, 50, 100 y ajustes de parámetros que resulten en AUC = 0.6, 0.75 y 0.9

Para lanzar los escenarios es primero correr el script `package_manager.R` para instalar las dependencias. Para lanzar las simulaciones simplemente hay que ejecutar el script `bias_rmse/simulate_all.R`. Los resultados se almacenarán en archivos JSON para ser transformados a trablas de latex en un .txt con el script de python `bias_rmse/table_generator.py`

## Potencias y niveles

Para estudiar las potencias y niveles de los tests asociados al estimador eta, estas se han calculado a través de un montecarlo de tamaño 1000, hallando los p valores de cada estimador calculados a través de un bootstrap de tamaño 500. Debido a la intesidad computacional del método, se ha optado por realizar los cálculos en Julia, optimizando las funciones de cada estimador y paralelizando el montecarlo con `@threads`. Se ha elaborado la librería local `Eta4Roc` en la que se definen todas las funciones necesarias optimizadas.

Los archivos generados también se guardan en sus respectivos JSON para luego ser convertidos a tablas de LaTeX.

## Paquetes
Para la gestión de archivos json, paths relativos y selección del parámetro de suavizado h se han utilizado los paquetes `jsonlite`, `here` y `ks` respectivamente. También se hace uso de la librería `pROC` para confirmar el cálculo correcto de AUCs. El script `package_manager.R"` se encarga de la gestión de estos.

**Es esencial ejecutar este archivo antes lanzar la primera simulación.**


  
