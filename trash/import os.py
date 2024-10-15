import os
import json
import pandas as pd

# Directorio donde están los archivos JSON
directorio = '/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/no_parametrico/jsons'

# Inicializa una lista para almacenar los datos de eta_pob
filas = []

# Iterar sobre cada archivo en el directorio
for archivo in os.listdir(directorio):
    if archivo.endswith('.json'):
        ruta_archivo = os.path.join(directorio, archivo)
        
        # Abrir y leer el archivo JSON
        with open(ruta_archivo, 'r') as f:
            datos = json.load(f)
            
            # Recorre cada AUC y tamaño en el JSON
            for auc, tamaños in datos.items():
                if isinstance(tamaños, dict):
                    for tamaño, valores in tamaños.items():
                        eta_pob = valores.get('eta_pob', [None])[0]  # Obtiene el valor de eta_pob
                        filas.append([archivo, auc, tamaño, eta_pob])  # Guarda la fila

# Crear un DataFrame con los datos obtenidos
df = pd.DataFrame(filas, columns=["Archivo", "AUC", "Tamaño", "eta_pob"])

# Generar la tabla en formato LaTeX
tabla_latex = df.to_latex(index=False)

# Guardar la tabla en un archivo .txt
with open('tabla_eta_pob.txt', 'w') as f:
    f.write(tabla_latex)

print("Tabla en formato LaTeX guardada en 'tabla_eta_pob.txt'.")
