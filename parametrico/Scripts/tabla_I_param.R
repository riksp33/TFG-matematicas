source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 1 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones_binormales = numeric(MC)
    
    #variables que recogen datos de la población
    mu_y = 2.5
    std_dev_y = sqrt(0.25)
    std_dev_x = sqrt(0.25)
    mu_x = ObtenerMux(auc , std_dev_x , std_dev_y , mu_y)
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    X_pob = rnorm(1e5 , mu_x , std_dev_x)
    Y_pob = rnorm(1e5 , mu_y , std_dev_y)
    eta_pob_binormal = eta_poblacional_I(mu_x)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob_binormal, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X = rnorm(n , mu_x , std_dev_x)
        Y = rnorm(n , mu_y , std_dev_y)
        estimaciones_binormales[i] = EtaBinormal(casos = Y, control = X)
      }

      bias_binormal = GetBias(estimaciones_binormales , eta_pob_binormal)
      rmse_binormal = GetRMSE(estimaciones_binormales , eta_pob_binormal)   
      
      nombre = paste0('size:',n)
      resultado_auc[[nombre]]=list(
        bias = bias_binormal,
        rmse = rmse_binormal
      )
      cat('las estimaciones para n =' , n , ' y AUC=' , auc, 'son: \n')
      cat('BIAS EMPÍRICO = ' , bias_binormal , '\n')
      cat('RMSE EMPÍRICO = ' , rmse_binormal , '\n')
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA)
  dir_path = here('parametrico', 'jsons')
  full_path = file.path(dir_path,'tabla_I_param.json' )
  write(json, file = full_path)
}


set.seed(1)
AUCs = c(0.7 , 0.8 , 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

