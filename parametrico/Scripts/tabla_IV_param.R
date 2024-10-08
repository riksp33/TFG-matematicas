
source('~/Desktop/ucm/TFGs/Mates/TFG-matematicas/funciones_auxiliares.R')

SimulateBiasRMSE = function(AUCs , tamaños){
  lista_json = list()
  lista_json['header'] = 'Simulación de la tabla 4 de Faraggi'
  
  for (auc in AUCs) {
    resultado_auc = list()
    
    #variables para hacer MC
    MC = 1000
    estimaciones = numeric(MC)
    
    #variables que recogen datos de la población
    mu_y = 2.5
    std_dev_y = sqrt(0.25)
    std_dev_x = sqrt(0.09)
    mu_x = ObtenerMux(auc , std_dev_x , std_dev_y , mu_y)
    
    #Simular la población real con tamaño 100.000 para tomar el eta "verdadero"
    # correspondiente a cada AUC y media de X
    X_pob = exp(rnorm(1e5 , mu_x , std_dev_x))
    Y_pob = exp(rnorm(1e5 , mu_y , std_dev_y))
    eta_pob = eta_poblacional_IV(mu_x)
    cat('===================================================================\n')
    cat('Los etas "verdaderos" para AUC = ' , auc, 'son: \n')
    cat('el eta empirico es: ', eta_pob, '\n')
    cat('===================================================================\n')
    
    for (n in tamaños) {
      for (i in 1:MC) {
        X = exp(rnorm(n , mu_x , std_dev_x))
        Y = exp(rnorm(n , mu_y , std_dev_y))
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
  write(json, file = 'tabla_IV_param.json')
}


set.seed(1)
AUCs = c(0.7 , 0.8 , 0.9)
ns = c(20 , 50 , 100)
SimulateBiasRMSE(AUCs , ns)

