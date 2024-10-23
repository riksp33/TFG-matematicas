source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de kernel de Faraggi, h paquete, gaussiano tabla 5 rate = 0.22'
  
    for (auc in AUCs) {
    resultado_auc = list()
    MC = 1000
    estimaciones_kernel = numeric(MC)
    X_pob = rgamma(1e5, shape = 2, rate = 0.24)
    Y_pob = rgamma(1e5, shape = 2, rate = 1)
    eta_pob_kernel = eta_poblacional_V_1()
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta kernel es: ', eta_pob_kernel, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (m in 1:MC) {
        
        X = rgamma(n, shape = 2, rate = 0.24)
        Y = rgamma(n, shape = 2, rate = 1)
        
        estimaciones_kernel[m] = EtaKernel(muestra_sanos= Y, muestra_enfermos= X, metodo = 'hscv', mesh_size = 300)
      }
      
      bias_kernel = GetBias(estimaciones_kernel , eta_pob_kernel)
      rmse_kernel = GetRMSE(estimaciones_kernel , eta_pob_kernel)

      
      nombre = paste0('size:',n)
      resultado_auc[[nombre]]=list(
        bias = bias_kernel,
        rmse = rmse_kernel
      )
      cat('las estimaciones para n =' , n , ' y AUC=' , auc, 'son: \n')
      cat('BIAS KERNEL = ' , bias_kernel , '\n')
      cat('RMSE KERNEL = ' , rmse_kernel , '\n')
   
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA)
  dir_path = here('kernel', 'gaussiano', 'h_paquete', 'jsons')
  full_path = file.path(dir_path,'tabla_V_1_kernel_h_funcion_gaussiano.json' )
  write(json, file = full_path)}


set.seed(1)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(c(0.9) , ns)

