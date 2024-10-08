source('~/Desktop/ucm/TFGs/Mates/codigo/codigo_final/funciones_auxiliares.R')


SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 6 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones = numeric(MC)
    
    #variables que recogen datos de la población
    mu_y = 0
    std_devy = 1
    std_devx = GetStdMixtura(var_a = 1, var_b = 5, p_a = 0.5)
    
    Y_pob = (rnorm(1e5 , mu_y , std_devy))
    mu_x = ObtenerMuMixtura(auc, Y_pob, 1, 5, offset = 4 )
    X_1 = rnorm(1e5, mu_x, 1)
    X_2 = rnorm(1e5, mu_x + 4, sqrt(5))
    X_pob = HacerMixtura(X_1, X_2, 0.5)
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    
    
    eta_pob = eta_poblacional_VI(mu_x, mu_x + 4, 1, sqrt(5), 0.5)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X_1_loop = rnorm(n, mu_x, 1)
        X_2_loop = rnorm(n, mu_x + 4, sqrt(5))
        X = HacerMixtura(X_1_loop, X_2_loop, 0.5)
        Y = (rnorm(n , mu_y , std_devy))
        
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
      cat('BIAS  = ' , bias , '\n')
      cat('RMSE  = ' , rmse , '\n')
      
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA)
  write(json, file = 'tabla_VI_param.json')
}


set.seed(1)
AUCs = c(0.7 , 0.8 , 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

