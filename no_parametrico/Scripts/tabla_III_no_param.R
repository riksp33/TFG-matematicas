source(here('funciones_auxiliares.R'))

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 3 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones_empiricas = numeric(MC)
    
    #variables que recogen datos de la población
    mu_y = 2.5
    std_dev_y = sqrt(0.09)
    std_dev_x = sqrt(0.25)
    mu_x = ObtenerMux(auc , std_dev_x , std_dev_y , mu_y)
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    X_pob = rnorm(1e5 , mu_x , std_dev_x)^3
    Y_pob = rnorm(1e5 , mu_y , std_dev_y)^3
    eta_pob_empirico = eta_poblacional_III(mu_x)
    auc_observado = as.numeric(pROC::auc(response = c(rep(1, 1e5), rep(0, 1e5)),
                                         predictor = c(Y_pob, X_pob)))
    
    
    observed_auc = as.numeric(pROC::auc(response = c(rep(1, 1e5), rep(0, 1e5)),
                                        predictor = c(Y_pob, X_pob)))
    stopifnot(abs(observed_auc - auc) <= 0.05)
    cat('===================================================================\n')
    cat('El AUC OBSERVADO es: ', auc_observado, '\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob_empirico, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X = rnorm(n , mu_x , std_dev_x)^3
        Y = rnorm(n , mu_y , std_dev_y)^3
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
  full_path = file.path(dir_path,'tabla_III_no_param.json' )
  write(json, file = full_path)
}


set.seed(1)
AUCs = c(0.7 , 0.8 , 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

