source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de kernel de Faraggi, h optimo, gaussiano tabla 5 rate = 0.22'
  
    for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones_kernel = numeric(MC)
    #variables que recogen datos de la población
    ##############################################################
    # mu_y = 2.5
    # std_dev_y = sqrt(0.09)
    # std_dev_x = sqrt(0.25)
    # mu_x = ObtenerMux(auc , std_dev_x , std_dev_y , mu_y)
    ################################################################
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    
    ###############################################################
    X_pob = rgamma(1e5, shape = 2, rate = 0.24)
    Y_pob = rgamma(1e5, shape = 2, rate = 1)
    ###############################################################
    eta_pob_kernel = eta_poblacional_V_1()
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta kernel es: ', eta_pob_kernel, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (m in 1:MC) {
        ###############################################
        X = rgamma(n, shape = 2, rate = 0.24)
        Y = rgamma(n, shape = 2, rate = 1)
        ###############################################
        estimaciones_kernel[m] = EtaKernel(muestra_sanos= Y, muestra_enfermos= X, metodo = 'optimo', mesh_size = 300)
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
  dir_path = here('kernel', 'gaussiano', 'h_optimo', 'jsons')
  full_path = file.path(dir_path,'tabla_V_1_kernel_h_optimo_gaussiano.json' )
  write(json, file = full_path)}


set.seed(1)
AUCs = c( 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

