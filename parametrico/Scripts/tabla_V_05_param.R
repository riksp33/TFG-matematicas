source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 5 0.5 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones = numeric(MC)
    
    #variables que recogen datos de la población

    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    X_pob = rgamma(1e5, shape = 2, rate = 0.12)
    Y_pob = rgamma(1e5, shape = 2, rate = 0.5)
    eta_pob = eta_poblacional_V_05()
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X = rgamma(n, shape = 2, rate = 0.12)
        Y = rgamma(n, shape = 2, rate = 0.5)
        estimaciones[i] = EtaBinormal(controles = Y, casos = X)
      }
      
      bias = GetBias(estimaciones , eta_pob)
      rmse = GetRMSE(estimaciones , eta_pob)
      
      nombre = paste0('size:',n)
      resultado_auc[[nombre]]=list(
        bias = bias,
        rmse = rmse
      )
      cat('las estimaciones para n =' , n , ' y AUC=' , auc, 'son: \n')
      cat('BIAS EMPÍRICO = ' , bias , '\n')
      cat('RMSE EMPÍRICO = ' , rmse , '\n')
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA)
  dir_path = here('parametrico', 'jsons')
  full_path = file.path(dir_path,'tabla_V_05_param.json' )
  write(json, file = full_path)}


set.seed(1)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(c(0.9) , ns)

