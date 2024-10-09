source(here('funciones_auxiliares.R'))


SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 5 1 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones_empiricas = numeric(MC)
    
    #variables que recogen datos de la población

    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    X_pob = rgamma(1e5, shape = 2, rate = 0.24)
    Y_pob = rgamma(1e5, shape = 2, rate = 1)
    eta_pob_empirico = eta_poblacional_V_1()
    
    observed_auc = as.numeric(pROC::auc(response = c(rep(1, 1e5), rep(0, 1e5)),
                                        predictor = c(Y_pob, X_pob)))
    stopifnot(abs(observed_auc - auc) <= 0.05)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob_empirico, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X = rgamma(n, shape = 2, rate = 0.24)
        Y = rgamma(n, shape = 2, rate = 1)
        estimaciones_empiricas[i] = EtaEmpirica(control = Y, casos = X)
      }
      
      bias_empirico = GetBias(estimaciones_empiricas , eta_pob_empirico)
      rmse_empirico = GetRMSE(estimaciones_empiricas , eta_pob_empirico)
      
      nombre = paste0('size:',n)
      resultado_auc[[nombre]]=list(
        bias = bias_empirico,
        rmse = rmse_empirico,
        eta_pob = eta_pob_empirico
      )
      cat('las estimaciones para n =' , n , ' y AUC=' , auc, 'son: \n')
      cat('BIAS EMPÍRICO = ' , bias_empirico , '\n')
      cat('RMSE EMPÍRICO = ' , rmse_empirico , '\n')
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA) 
  dir_path = here('no_parametrico', 'jsons')
  full_path = file.path(dir_path,'tabla_V_1_no_param.json' )
  write(json, file = full_path)
}


set.seed(1)
AUCs = c(0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

