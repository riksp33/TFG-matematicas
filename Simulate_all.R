AUCs = c(0.6 , 0.75 , 0.9)

source(here('no_parametrico', 'Simulate.R'))
rm(list = ls())
AUCs = c(0.6 , 0.75 , 0.9)

source(here('parametrico', 'Simulate.R'))
rm(list = ls())
AUCs = c(0.6 , 0.75 , 0.9)

source(here('kernel', 'gaussiano', 'h_optimo', 'Simulate.R'))
rm(list = ls())
AUCs = c(0.6 , 0.75 , 0.9)

source(here('kernel', 'gaussiano', 'h_paquete', 'Simulate.R'))
rm(list = ls())


