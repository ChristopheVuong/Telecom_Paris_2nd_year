## Partie 1

source('mesfonctions.R')
load('projectData.Rdata')
source('evalTools.R')

## 1.(a)
u = interpolLagrange(n = 2, a = 1, b = 2.5, neval = 2000, nodes = "equi", FUN = evalHeight, Plot = TRUE)
u = interpolLagrange(n = 70, a = 1, b = 2.5, neval = 2000, nodes = "equi", FUN = evalHeight, Plot = TRUE)

u = interpolLagrange(n = 3, a = 1, b = 2.5, neval = 2000, nodes = "cheby", FUN = evalHeight, Plot = TRUE)
u = interpolLagrange(n = 50, a = 1, b = 2.5, neval = 1500, nodes = "cheby", FUN = evalHeight, Plot = TRUE)

## 1.(b)
u = interpolLagrange(n = 30, a = 1, b = 2.5, neval = 2000, nodes = "cheby", FUN = evalHeight, Plot = TRUE)

## 1.(c)
errMinOrder(orderMax = 100, a = 1, b = 2.5, neval = 10^4, nodes = "equi", FUN = evalHeight)
errMinOrder(orderMax = 100, a = 1, b = 2.5, neval = 10^4, nodes = "cheby", FUN = evalHeight)

## 2.(a)
piece = piecewiseInterpol(n = 16, nInt = 4, a = 1, b = 2.5, neval = 100, nodes = "equi", FUN = evalHeight, Plot = TRUE)

## 2.(b)
errMinPiece(budget = 80, a = 1, b = 2.5, neval = 10^4, FUN = evalHeight)
piece = piecewiseInterpol(n = 34, nInt = 2, a = 1, b = 2.5, neval = 100, nodes = "cheby", FUN = evalHeight, Plot = TRUE)
