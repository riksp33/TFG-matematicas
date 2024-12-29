import json
import os
os.chdir('./niveles_y_potencias/two_biomarkers/niveles')

def json_to_latex_table(json_files, output_file):
    auc_values = ["0.7", "0.8", "0.9"]
    correlations = ["0.0", "0.5"]
    measures = [
        "$\\widehat{AUC}$",
        "$\\widehat{J}$",
        "$\\hat{\\eta}_{log}^{N}$",
        "$\\hat{\\eta}_{log}^{K = g, h=csv}$",
        "$\\hat{\\eta}_{log}^{K = g, h = h*}$"
    ]

    latex_code = []
    latex_code.append("\\begin{table}[H]")
    latex_code.append("\\centering")
    latex_code.append("\\caption{}")
    latex_code.append("\\renewcommand{\\arraystretch}{1.5} % Incrementa el espaciado entre filas")
    latex_code.append("\\begin{tabular}{lllccc}")
    latex_code.append("\\multicolumn{3}{c}{}  & \\multicolumn{3}{c}{\\(n_x = n_y\\)} \\\\")
    latex_code.append("\\cmidrule(lr){4-6}")
    latex_code.append("AUC & Correlacion &   & 20 & 50 & 100 \\\\")
    latex_code.append("\\midrule")

    for auc_idx, json_file in enumerate(json_files):
        with open(json_file, 'r') as file:
            data = json.load(file)
        
        for corr in correlations:
            corr_key = f"correlation_{corr}"
            if corr_key in data:
                for measure_idx, measure in enumerate(measures):
                    row_values = []
                    for sample_size in ["20", "50", "100"]:
                        sample_key = f"N_{sample_size}"
                        if sample_key in data[corr_key]:
                            if measure_idx == 0:  
                                row_values.append(data[corr_key][sample_key]["auc"])
                            elif measure_idx == 1:  
                                row_values.append(data[corr_key][sample_key]["youden"])
                            elif measure_idx == 2:  
                                row_values.append(data[corr_key][sample_key]["kernel_hscv"])
                            elif measure_idx == 3:  
                                row_values.append(data[corr_key][sample_key]["param"])
                            elif measure_idx == 4:  
                                row_values.append(data[corr_key][sample_key]["kernel_opt"])
                        else:
                            row_values.append("N/A")
                    
                    if measure_idx == 0:
                        latex_code.append(f"{auc_values[auc_idx]} & $\\rho =$ {corr} & {measure} & {' & '.join(map(str, row_values))} \\\\")
                    else:
                        latex_code.append(f"    &              & {measure} & {' & '.join(map(str, row_values))} \\\\")
                latex_code.append("[0.2cm]")  
        latex_code.append("&&&&&\\\\")  

    latex_code.append("\\bottomrule")
    latex_code.append("\\end{tabular}")
    latex_code.append("\\label{tab:simulated_power3}")
    latex_code.append("\\end{table}")

    with open(output_file, 'w') as file:
        file.write('\n'.join(latex_code))

json_files = ["nivel_1.json", "nivel_2.json", "nivel_3.json"]  
json_files = ['./jsons/' + file for file in json_files]  
output_file = "tabla_latex.txt"
json_to_latex_table(json_files, output_file)
