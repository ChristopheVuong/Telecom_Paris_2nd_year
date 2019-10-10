## Partie 2

source('mesfonctions.R')
load('projectData.Rdata')
source('evalTools.R')


# 1. Estimation

tab = (30:100)
for (M in (30:100)){
  tab[M - 29] = simpsonInt(FUN = evalHeight, a = 1, b = 2.5, M)
}
tab


# 1.(b) Pour M = 2*10 (l'erreur est donné en 2M)
evalErrSimpson(FUN = evalHeight,a = 1, b = 2.5, M = 10)[1]


# 2.

precisionMSimspon(FUN = evalHeight, a = 1, b = 2.5, init = 1, precision = 0.00005) 
#Pour M = 2*128 (l'erreur est donnée en 2M)
evalErrSimpson(FUN = evalHeight, a = 1, b = 2.5, M = 128)[1]
