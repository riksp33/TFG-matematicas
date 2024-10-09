import json

def cargar_datos(archivo):
    with open(archivo, 'r') as f:
        return json.load(f)

def generar_tabla_latex(tipo, datos1, datos2, datos3, datos4, titulo):

    tabla = r'''
\begin{table}[H]
\centering
\caption{Simulación de ''' + tipo + r''' para tamaños muestrales de ''' +  titulo + r'''}
\setlength{\tabcolsep}{11pt} % Ajusta el espacio entre las columnas
\begin{tabularx}{0.8\textwidth}{c c *{4}{>{\centering\arraybackslash}X}}
\toprule
'''
    if 'eta_pob' in datos1['AUC:0.7']['size:20']:
        tabla += "n & $\\eta_{sd}$ & $\hat{\eta}_{sd}^{NonParametric}$ & $\hat{\eta}_{sd}^{N}$ & $\hat{\eta}_{sd}^{K = g, h=csv}$ & $\hat{\eta}_{sd}^{K = g, h = h*}$ \\\\\n"
    else:
        tabla += "n & AUC & $\hat{\eta}_{sd}^{NonParametric}$ & $\hat{\eta}_{sd}^{N}$ & $\hat{\eta}_{sd}^{K = g, h=csv}$ & $\hat{\eta}_{sd}^{K = g, h = h*}$ \\\\\n"
    
    tabla += "\\midrule\n"

    aucs = ["0.7", "0.8", "0.9"]
    sizes = ["20", "50", "100"]
    
    for size in sizes:
        for auc in aucs:
            if 'eta_pob' in datos1[f'AUC:{auc}'][f'size:{size}']:
                eta_pob = datos1[f'AUC:{auc}'][f'size:{size}']['eta_pob'][0]
                tabla += f"{size} & {eta_pob:.6f} & "
            else:
                tabla += f"{size} & {auc} & "

            tabla += f"{datos1[f'AUC:{auc}'][f'size:{size}'][tipo][0]:.4f} & "
            tabla += f"{datos2[f'AUC:{auc}'][f'size:{size}'][tipo][0]:.4f} & "
            tabla += f"{datos3[f'AUC:{auc}'][f'size:{size}'][tipo][0]:.4f} & "
            tabla += f"{datos4[f'AUC:{auc}'][f'size:{size}'][tipo][0]:.4f} \\\\\n"
        
        tabla += "\\midrule\n"

    tabla += r'''
\end{tabularx}
\label{extended_table}
\end{table}
'''
    return tabla


def gen_all_tables():
    ns = ['I', 'II', 'III', 'IV', 'VI', 'VII', 'VIII', 'IX']
    titulos = {
        'I' : '$Y \\sim N(2.5, 0.25)$, $X\\sim N(\\mu_x, 0.25)$',
        'II' : '$Y \\sim N(2.5, 0.09)$, $X\\sim N(\\mu_x, 0.25)$',
        'III': '$Y^{1/3} \\sim N(2.5, 0.09)$, $X^{1/3} \\sim N(\\mu_x, 0.25)$',
        'IV' : '$\\log(Y) \\sim N(2.5, 0.25)$, $\\log(X) \\sim N(\\mu_x, 0.09)$',
        'VI': '$Y \\sim N(0, 1)$, $X \\sim 0.5N(\\mu_x,1) + 0.5N(\\mu_x + 4, 5)$',
        'VII': '$Y \\sim N(0, 1)$, $X \\sim 0.5N(\\mu_x,1) + 0.5N(\\mu_x + 8, 5)$',
        'VIII' : '$Y  \\sim 0.5N(0, 1) + 0.5N(3,1)$, $X \\sim 0.5N(\\mu_x,1) + 0.5N(\\mu_x + 4, 5)$',
        'IX' : '$Y  \\sim 0.5N(0, 1) + 0.5N(3,1.5)$, $X \\sim 0.5N(\\mu_x,1) + 0.5N(\\mu_x + 4, 5)$'

    }

    for n in ns:
        archivo1 = f'./no_parametrico/jsons/tabla_{n}_no_param.json'  # NonParametric (contiene eta_pob)
        archivo2 = f'./parametrico/jsons/tabla_{n}_param.json'  # N
        archivo3 = f'./kernel/gaussiano/h_paquete/jsons/tabla_{n}_kernel_h_optimo_gaussiano.json'  # K = g, h=csv
        archivo4 = f'./kernel/gaussiano/h_optimo/jsons/tabla_{n}_kernel_h_optimo_gaussiano.json'  # K = g, h=h*  

        datos1 = cargar_datos(archivo1)
        datos2 = cargar_datos(archivo2)
        datos3 = cargar_datos(archivo3)
        datos4 = cargar_datos(archivo4)   


        tabla_bias = generar_tabla_latex("bias", datos1, datos2, datos3, datos4, titulos[n])
        tabla_rmse = generar_tabla_latex("rmse", datos1, datos2, datos3, datos4, titulos[n]) 

        with open(f'./tablas/tabla_{n}.txt', 'w') as f:
            f.write(tabla_bias)
            f.write("\n\n")      
            f.write(tabla_rmse)          



gen_all_tables()


def generar_tabla_5(datos_lambda_05, datos_lambda_1, tipo, titulo):
    tabla = r'''
\begin{table}[H]
\centering
\caption{''' + titulo + r'''}
\setlength{\tabcolsep}{11pt} % Ajusta el espacio entre las columnas
\begin{tabularx}{0.8\textwidth}{c c *{4}{>{\centering\arraybackslash}X}}
\toprule
n & $\lambda_Y$ & $\hat{\eta}_{sd}^{NonP}$ & $\hat{\eta}_{sd}^{N}$ & $\hat{\eta}_{sd}^{K, h=csv}$ & $\hat{\eta}_{sd}^{K, h*}$ \\
\midrule
'''


    sizes = ["20", "50", "100"]
    
    # Iteramos sobre tamaños muestrales para lambda = 0.5
    for size in sizes:
        tabla += f"{size} & 0.5 & "
        tabla += f"{datos_lambda_05[0]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # NonP
        tabla += f"{datos_lambda_05[1]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # N
        tabla += f"{datos_lambda_05[2]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # K, h=csv
        tabla += f"{datos_lambda_05[3]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} \\\\\n"  # K, h=h*
    
    tabla += "\\midrule\n"

    # Iteramos sobre tamaños muestrales para lambda = 1
    for size in sizes:
        tabla += f"{size} & 1 & "
        tabla += f"{datos_lambda_1[0]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # NonP
        tabla += f"{datos_lambda_1[1]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # N
        tabla += f"{datos_lambda_1[2]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} & "  # K, h=csv
        tabla += f"{datos_lambda_1[3]['AUC:0.9'][f'size:{size}'][tipo][0]:.4f} \\\\\n"
    
    tabla += r'''
\bottomrule
\end{tabularx}
\label{extended_table}
\end{table}
'''
    return tabla

# Archivos JSON para lambda = 0.5 y lambda = 1
archivos_lambda_05 = [
    './no_parametrico/jsons/tabla_V_05_no_param.json',  # NonParametric
    './parametrico/jsons/tabla_V_05_param.json',  # N
    './kernel/gaussiano/h_paquete/jsons/tabla_V_05_kernel_h_optimo_gaussiano.json',  # K, h=csv
    './kernel/gaussiano/h_optimo/jsons/tabla_V_05_kernel_h_optimo_gaussiano.json'   # K, h=h*
]

archivos_lambda_1 = [
    './no_parametrico/jsons/tabla_V_1_no_param.json',  # NonParametric
    './parametrico/jsons/tabla_V_1_param.json',  # N
    './kernel/gaussiano/h_paquete/jsons/tabla_V_1_kernel_h_optimo_gaussiano.json',  # K, h=csv
    './kernel/gaussiano/h_optimo/jsons/tabla_V_1_kernel_h_optimo_gaussiano.json'   # K, h=h*
]

datos_lambda_05 = [cargar_datos(archivo) for archivo in archivos_lambda_05]
datos_lambda_1 = [cargar_datos(archivo) for archivo in archivos_lambda_1]

titulo_bias = r"Simulación de Bias de \textbf{$\hat{\eta}_{sd}$} para tamaños muestrales y valor de AUC 0.9: $Y \sim \gamma(\lambda_Y,\ \alpha_Y = 2)$, $X \sim \gamma(\lambda_X,\ \alpha_X = 2)$"
tabla_bias = generar_tabla_5(datos_lambda_05, datos_lambda_1, "bias", titulo_bias)

titulo_rmse = r"Simulación de $RMSE$ de \textbf{$\hat{\eta}_{sd}$} para tamaños muestrales y valor de AUC 0.9: $Y \sim \gamma(\lambda_Y,\ \alpha_Y = 2)$, $X \sim \gamma(\lambda_X,\ \alpha_X = 2)$"
tabla_rmse = generar_tabla_5(datos_lambda_05, datos_lambda_1, "rmse", titulo_rmse)

with open('./tablas/tabla_V.txt', 'w') as f:
    f.write(tabla_bias)
    f.write("\n\n")
    f.write(tabla_rmse)



import os

def unir_archivos_txt(ruta_carpeta, archivo_salida):
    patrones = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
    
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

# Ruta de la carpeta donde están los archivos .txt
ruta_carpeta = "./tablas"

# Nombre del archivo de salida
archivo_salida = "./tablas/tablas_simulacion.txt"

# Llamar a la función para unir los archivos
unir_archivos_txt(ruta_carpeta, archivo_salida)

print(f"Archivo combinado creado: {archivo_salida}")
