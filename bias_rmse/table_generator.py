import json
import os
import os

os.chdir('/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/bias_rmse')
print("Directorio actual:", os.getcwd())

# GLOBALES
ruta_carpeta_txts = "./tablas"
archivo_salida_txt = "./tablas/tablas_simulacion_altered.txt"


def cargar_datos(archivo):
    with open(archivo, 'r') as f:
        return json.load(f)

def generar_tabla_latex(tipo, datos, titulo):
    if tipo == 'rmse':
        tipo_titulo = '$RMSE$'
    else:
        tipo_titulo = 'Bias'

    tabla = r'''
\begin{table}[H]
\centering
\caption{Simulación de ''' + tipo_titulo + r''' para diferentes tamaños muestrales de ''' +  titulo + r'''}
\setlength{\tabcolsep}{11pt} % Ajusta el espacio entre las columnas
\begin{tabularx}{0.8\textwidth}{c c *{4}{>{\centering\arraybackslash}X}}
\toprule
'''

    # Verificamos si contiene eta_pob
    sample_auc = next(iter(datos.keys()))
    if sample_auc == 'header':  # Saltamos la key 'header' si existe
        sample_auc = next(key for key in datos.keys() if key != 'header')
    sample_size = next(iter(datos[sample_auc].keys()))
    
    if 'eta_pob' in datos[sample_auc][sample_size]:
        tabla += "$\\eta_{log}$ & n & $\\hat{\eta}_{log}^{NonParametric}$ & $\\hat{\eta}_{log}^{N}$ & $\\hat{\eta}_{log}^{K = g, h=csv}$ & $\\hat{\eta}_{log}^{K = g, h = h*}$ \\\\\n"
    else:
        tabla += "n & AUC & $\\hat{\eta}_{log}^{NonParametric}$ & $\\hat{\eta}_{log}^{N}$ & $\\hat{\eta}_{log}^{K = g, h=csv}$ & $\\hat{\eta}_{log}^{K = g, h = h*}$ \\\\\n"
    
    tabla += "\\midrule\n"

    aucs = ["AUC:0.6", "AUC:0.75", "AUC:0.9"]
    sizes = ["size:20", "size:50", "size:100"]
    
    for auc in aucs:
        for index, size in enumerate(sizes):
            if 'eta_pob' in datos[auc][size]:
                eta_pob = datos[auc][size]['eta_pob'][0]
                if index != 1:
                    tabla += f' & {size.split(":")[1]} &'
                else:
                    tabla += f'{eta_pob:.6f} & {size.split(":")[1]} & '
            else:
                tabla += f'{size.split(":")[1]} & {auc.split(":")[1]} & '

            # Extraemos los valores de bias/rmse para cada método
            no_param = datos[auc][size]['no_param'][tipo][0]
            param = datos[auc][size]['param'][tipo][0]
            kernel_hscv = datos[auc][size]['kernel_hscv'][tipo][0]
            kernel_opt = datos[auc][size]['kernel_opt'][tipo][0]

            tabla += f"{no_param:.4f} & {param:.4f} & {kernel_hscv:.4f} & {kernel_opt:.4f} \\\\\n"
        
        tabla += "\\midrule\n"

    tabla += r'''
\end{tabularx}
\label{extended_table}
\end{table}
'''
    return tabla

def gen_all_tables():
    ns = ['I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X']  # Incluimos todos los escenarios
    titulos = {
        'I': '$Y \\sim N(0, 1)$, $X\\sim N(\\mu_x, 1)$',
        'II': '$Y \\sim N(0, 1)$, $X\\sim N(\\mu_x, 1.4)$',
        'III': '$Y \\sim N(0, 1)$, $X \\sim N(\\mu_x, 0.3)$',
        'IV': '$\\log(Y) \\sim N(0, 1)$, $\\log(X) \\sim N(\\mu_x, 0.5)$',
        'V': '$\\log(Y) \\sim N(0, 1)$, $\\log(X) \\sim N(\\mu_x, \\frac{3}{2})$',
        'VI': '$\\log(Y) \\sim N(0, 1)$, $\\log(X) \\sim N(\\mu_x, 0.2)$',
        'VII': '$\\log(Y) \\sim N(0, 1)$, $\\log(X) \\sim N(\\mu_x, 2)$',
        'VIII': '$Y \\sim \\Gamma(0.5, 0.5)$, $X \\sim \\Gamma(\\lambda_x, 1)$' ,
        'IX': '$Y \\sim \\Gamma(0.5, 0.5)$, $X \\sim \\Gamma(\\lambda_x, 4)$',
        'X':'$Y \\sim \\Gamma(0.5, 0.5)$, $X \\sim \\Gamma(\\lambda_x, \\frac{1}{8})$' 
    }
    for n in ns:
        archivo = f'./jsons/tabla_{n}_nuevas_pob.json'
        print(os.path.isfile('./jsons/tabla_IV_nuevas_pob.json'))
        try:
            datos = cargar_datos(archivo)
            tabla_bias = generar_tabla_latex("bias", datos, titulos[n])
            tabla_rmse = generar_tabla_latex("rmse", datos, titulos[n])

            with open(f'./tablas/tabla_{n}.txt', 'w') as f:
                f.write(tabla_bias)
                f.write("\n\n")      
                f.write(tabla_rmse)
                
        except FileNotFoundError:
            print(f"Advertencia: El archivo {archivo} no existe.")

def unir_archivos_txt(ruta_carpeta, archivo_salida):
    patrones = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", 'X']
    
    with open(archivo_salida, 'w') as archivo_final:
        for patron in patrones:
            nombre_archivo = f"tabla_{patron}.txt"
            ruta_archivo = os.path.join(ruta_carpeta, nombre_archivo)
            
            if os.path.exists(ruta_archivo):
                with open(ruta_archivo, 'r') as archivo:
                    contenido = archivo.read()
                    archivo_final.write(contenido)
                    archivo_final.write("\n\n")
            else:
                print(f"Advertencia: El archivo {nombre_archivo} no existe.")

# Generar todas las tablas y unirlas
gen_all_tables()
unir_archivos_txt(ruta_carpeta_txts, archivo_salida_txt)

print(f"Archivo combinado creado: {archivo_salida_txt}")

