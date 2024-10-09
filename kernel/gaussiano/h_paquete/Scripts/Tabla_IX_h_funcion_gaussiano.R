source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de kernel de Faraggi, h paquete, gaussiano tabla 9'
    for (auc in AUCs) {
    resultado_auc = list()
    
    MC = 1000
    estimaciones_kernel = numeric(MC)
    
    Y_1 = rnorm(1e5, 0, 1)
    Y_2 = rnorm(1e5, 3, sqrt(1.5))
    Y_pob = HacerMixtura(Y_1, Y_2, 0.5)

    mu_x = ObtenerMuMixtura(auc, Y_pob, 1, 5, offset = 4 )
    X_1 = rnorm(1e5, mu_x, 1)
    X_2 = rnorm(1e5, mu_x + 4, sqrt(5))
    X_pob = HacerMixtura(X_1, X_2, 0.5)

    observed_auc = as.numeric(pROC::auc(response = c(rep(1, 1e5), rep(0, 1e5)),
                                        predictor = c(Y_pob, X_pob)))
    stopifnot(abs(observed_auc - auc) <= 0.05)
    
    eta_pob_kernel = eta_poblacional_IX(mu_x, mu_x + 4, 1, sqrt(5), 0.5)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta kernel es: ', eta_pob_kernel, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (m in 1:MC) {
        
        X_1_loop = rnorm(n, mu_x, 1)
        X_2_loop = rnorm(n, mu_x + 4, sqrt(5))
        X = HacerMixtura(X_1_loop, X_2_loop, 0.5)
        
        Y_1_loop = rnorm(n, 0, 1)
        Y_2_loop = rnorm(n, 3, sqrt(1.5))
        Y = HacerMixtura(Y_1_loop, Y_2_loop, 0.5)     

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
  full_path = file.path(dir_path,'tabla_IX_kernel_h_funcion_gaussiano.json' )
  write(json, file = full_path)}


set.seed(1)
AUCs = c(0.7, 0.8, 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

