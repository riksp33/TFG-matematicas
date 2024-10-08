source('~/Desktop/ucm/TFGs/Mates/codigo/codigo_final/funciones_auxiliares.R')


SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 7 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones_empiricas = numeric(MC)
    
    #variables que recogen datos de la población
    mu_y = 0
    std_devy = 1
    std_devx = GetStdMixtura(var_a = 1, var_b = 5, p_a = 0.5)
    
    Y_pob = (rnorm(1e5 , mu_y , std_devy))
    mu_x = ObtenerMuMixtura(auc, Y_pob, 1, 5, offset = 8 )
    X_1 = rnorm(1e5, mu_x, 1)
    X_2 = rnorm(1e5, mu_x + 8, sqrt(5))
    X_pob = HacerMixtura(X_1, X_2, 0.5)
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    
    
    eta_pob_empirico = eta_poblacional_VII(mu_x, mu_x + 8, 1, sqrt(5), 0.5)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob_empirico, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X_1_loop = rnorm(n, mu_x, 1)
        X_2_loop = rnorm(n, mu_x + 8, sqrt(5))
        X = HacerMixtura(X_1_loop, X_2_loop, 0.5)
        Y = (rnorm(n , mu_y , std_devy))
        
        estimaciones_empiricas[i] = EtaBinormal(controles = Y, casos = X)
      }
      
      bias_empirico = GetBias(estimaciones_empiricas , eta_pob_empirico)
      rmse_empirico = GetRMSE(estimaciones_empiricas , eta_pob_empirico)
      
      nombre = paste0('size:',n)
      resultado_auc[[nombre]]=list(
        bias = bias_empirico,
        rmse = rmse_empirico
      )
      cat('las estimaciones para n =' , n , ' y AUC=' , auc, 'son: \n')
      cat('BIAS EMPÍRICO = ' , bias_empirico , '\n')
      cat('RMSE EMPÍRICO = ' , rmse_empirico , '\n')
      
      cat('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n')
    }
    lista_json[[paste0('AUC:', auc)]] = resultado_auc
    
  }
  json = toJSON(lista_json, pretty = T, digits = NA)
  write(json, file = 'tabla_VII_param.json')
}


set.seed(1)
AUCs = c(0.7 , 0.8 , 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

