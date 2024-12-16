using Eta4Roc
using RCall
n= 10000
controles = rand(LogNormal(0,1), n)
casos = rand(LogNormal(0.26, 0.3 ), n)


R"""
source("/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/package_manager.R")
source("/Users/Riki/Desktop/ucm/TFGs/Mates/TFG-matematicas/funciones_auxiliares.R")
"""

@rput controles
@rput casos

auc_base = auc(controles, casos)
youden_base = youden(controles, casos)
param_base = parametric_eta(controles, casos, bc = true)
kernel_opt_base = eta_kernel(controles, casos, "optimo")
kernel_hscv_base = eta_kernel(controles, casos, "hscv")

param_r = rcopy(R"EtaBinormal(controles, casos, bc = T)")
kernel_opt_r = rcopy(R"EtaKernel(controles, casos, metodo = 'optimo')")
kernel_hscv_r = rcopy(R"EtaKernel(controles, casos, metodo = 'hscv')")


println("====================================")
println("Parametrico Julia: ", param_base, " Parametrico R: ", param_r, "diff = ", round((param_base - param_r), digits = 4))
println("Kernel opt Julia: ", kernel_opt_base, " Kernel opt R: ", kernel_opt_r, "diff = ", round((kernel_opt_base - kernel_opt_r), digits = 4))
println("Kernel hscv Julia: ", kernel_hscv_base, " Kernel hscv R: ", kernel_hscv_r, "diff = ", round((kernel_hscv_base - kernel_hscv_r), digits = 4))
println("====================================")
