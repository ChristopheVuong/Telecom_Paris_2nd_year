## Partie 3

source('mesfonctions.R')
load('projectData.Rdata')
source('evalTools.R')

n = 15
rich = richardson(FUN = evalHeight, n, t = 1, delta = 0.5, start = 1, Plot = TRUE)
rich
abline(v = c(2.5,10.5), col="green", lty= c(3,3), lwd = c(2,2))


rich[11]