##################################
######## Mes fonctions ##############
##################################


##################################
######## Partie 1 ##############
######## Interpolation polynomiale ####

dividif = function(x,y) {
  ##  Newton's Divided differences
  ##' @param x: a vector containing the interpolation nodes 
  ##' @param y: a vector of same size as x:
  ##           values of the interpolated function at the nodes
  ##' @return : a vector of same size as x:
  ##          the divided differences
  ##          \eqn{f_[x_0, ... x_k]} of order 'length(x) -1'. 
  n = length(x) - 1
  d = y
  if (n==0) {return(d)}
  for (j in 2:(n+1)) {
    d[j :(n+1)] = (d[j:(n+1)] - d[(j-1):n])/(x[j:(n+1)] - x[1:(n+2-j)])
  }
  return(d)
}


hornerNewton = function(a,x,z){
  ## Horner's method: Evaluates  a polynom P at points z, given
  ## nodes x and the coefficients a of P in Newton's basis
  ##
  ##' @param a : vector: the  coefficients of the polynomial in
  ##           Newton's basis
  ##' @param x : the interpolation nodes. 
  ##' @param z : vector of points where the polynom needs to be
  ##            evaluated. 
  ##' @return  : a vector of same size as z: the value of the
  ##            polynomial at points z.
  ##
  n <- length(x) - 1 ## degree of the Lagrange poynomial 
  if( (n< 0) || (length(a) != (n+1)) )
  {
    stop('at least one interpolating point is needed,
         a and x should have same length')
  }
  f <- a[n+1]*rep(1,length(z))
  if(n >= 1){
    for( k in 1:n){
      f = a[n+1-k] + f * (z - x[n+1-k])
    }
  }
  return(f)
  }


interpolDividif=function(x,y,z){
  ## Efficient Lagrange interpolation using Horner's method with  
  ## Newton basis for evaluation
  ##' @param x : vector containing the interpolation nodes 
  ##' @param y : vector of same size as x: values of the interpolated
  ##            function at the nodes
  ##' @param z : vector of points where the  interpolating polynomial
  ##            needs to be evaluated. 
  ##' @return  : vector of same size as z: the value of the
  ##            interpolating polynomial at points z.
  
  a = dividif(x,y)
  return (hornerNewton(a,x,z))
}

interpolLagrange =function(n,a,b,neval,nodes,FUN,Plot){
  ## Generic Lagrange interpolation, with equidistant or Chebyshev nodes. 
  ##' @param n : the degree of the interpolating polynomial 
  ##' @param a : left end-point of the interval
  ##' @param b : right end-point of the interval
  ##' @param neval :number of evaluation points (a regular grid will be
  ## used on [a,b]
  ##' @param nodes :string, either "equi" (default) for equidistant
  ## Lagrange interpolation (on each subinterval) or "cheby" for
  ## using Chebyshev nodes.
  ##' @param FUN: the function to be interpolated 
  ##' @param Plot : logical. Setting 'Plot' to TRUE produces a plot
  ## showing the graph of
  ## the true functions and its interpolation.  
  ##' @return : vector of size 'neval': the values of the Lagrange
  ## polynomial on an equi-distant or Chebyshev grid.
  
  if (nodes == "equi"){
    x =  seq(a,b,length.out = n+1)
  }
  else if (nodes == "cheby"){
    x = (b-a) * cos(((0:n) + 1/2)/(n+1)*pi)/2 + (a+b)/2
  }
  else{stop("the nodes must be either 'equi' or 'cheby'") }

  z = seq(a,b,length.out = neval)
  y = FUN(x)
  f = interpolDividif(x,y,z)
  
  if( Plot ){
    if (nodes == "equi"){ methodName = " equidistant "}
    else {   methodName = " Chebyshev "}
    
    plot(z, sapply(z,FUN), type="l", ylim=range(c(y,f)) )
    title(main = paste("Lagrange interpolation with ",
                       toString(n+1), methodName,
                       " nodes", sep=""))
    lines(z,f, col = 'red') 
    
    legend('topright', legend=c('function','interpolation'),
           col = c('black','red'), lwd=1)
    
  }
  return(f)              
}


piecewiseInterpol=function(n,nInt,a,b,neval, nodes = "equi", FUN, Plot){
  ##' @param n : the degree of the interpolating polynomial on each
  ## subinterval
  ##' @param nInt :  the number of sub-intervals
  ##' @param a, b : endpoints of the interval
  ##' @param neval : the number of points on the interpolating grid (on
  ## each subinterval)
  ##' @param nodes : string, either "equi" (default) for equidistant
  ## Lagrange interpolation (on each subinterval) or "cheby" for
  ## chebyshev nodes.
  ##' @param FUN the function to be interpolated
  ##' @param Plot : logical. Should the result be plotted ?
  ##' @return : a matrix with 2 rows and neval * nInt -neval + 1:
  ## values of the interpolated funtion on a regular grid (first row)
  ## and the corresponding abscissas (second row).
  
  intEndPoints = seq(a,b,length.out = nInt+1)
  f = c()
  z = c()
  for (m in 1:nInt){
    A = intEndPoints[m]; B = intEndPoints[m+1] 
    
    fm = interpolLagrange(n,A,B, neval,nodes, FUN, Plot = FALSE)
    zm = seq(A,B, length.out = neval)
    
    if( m >= 2){
      fm = fm[2:length(fm)]
      zm = zm[2:length(zm)]
    }
    z = c(z,zm)
    f = c(f,fm)
    
  }
  
  if (Plot == 1){
    if (nodes == "equi") {methodName = " equidistant "}
    else  {methodName = " Chebyshev "}
    
    
    plot(z, sapply(z,FUN),type="l")
    title(main = paste("Piecewise  Lagrange  interpolation with ",
                       toString(n+1), methodName, " nodes  on ",
                       toString(nInt), " Intervals", sep=""))
    lines(z,f, col='red', lwd=2)
    legend('topright', legend = c('function','interpolation'),
           lwd=c(1,2), col=c('black','red'))
  }
  return(rbind(f,z))
}


errMinOrder = function(orderMax = 100,a = 1,b = 2.5,neval,nodes,FUN){
  ##  Find the order of polynomial expression that minimizes the norm 
  ##  of difference with the FUN function
  ##' @param orderMax : highest order to test
  ##' @param a, b : interval end points 
  ##' @param neval : number of evaluations of the error
  ##' @param FUN : the function of reference
  ##' @return: the lowest order that minimizes the interpolation error.  
  
  err = (2:orderMax)
  grid = seq(a,b, length.out = neval)
  f = FUN(grid)
  for (order in 2:orderMax){
    v =interpolLagrange(order, a, b, neval, nodes, FUN, Plot = FALSE)
    ## compute the sup norm
    err[order-1] = max(abs(v - f))
    #print(err[order-1] + order)
  } 
  orderMin = which.min(err)+1
  return(paste("Order_min= ", toString(orderMin), "Error= " , toString(err[orderMin-1])))
}

errMinParam = function(budget = 80,a,b,neval = 10^4,nodes,FUN){
  ##  Find the couple (nInt, n_points) that minimizes the piecewise
  ##  interpolation error depending of the type of nodes
  ##' @param budget : the number of evaluations allowed for interpolation
  ##' @param a, b : interval end points 
  ##' @param neval : the max number of evaluations in total
  ##' @param nodes : string to indicate the type of nodes
  ##' @param FUN : the function of reference
  ##' @return: the lowest number of nodes that minimizes 
  ##' the interpolation error and the number of pieces.  
  
  piece = piecewiseInterpol(1, 1, a, b, neval, nodes, FUN, FALSE)
  f = FUN(piece[2,])
  v = piece[1,]
  mini = max(abs(v - f))
  index1 = 1
  index2 = 1
  for (nPoints in 2:budget){
    for (nInt in 1:(budget %/% nPoints)){
      ## (neval - 1) %/%nInt + 1 gives the maximum number of evaluations for test less or equal to 10^4
      ## piece = piecewiseInterpol(nPoints - 1, nInt, a, b, (neval - 1) %/%nInt + 1, nodes, FUN, FALSE)
      piece = piecewiseInterpol(nPoints - 1, nInt, a, b, (neval - 1) %/%nInt + 1, nodes, FUN, FALSE)
      f = FUN(piece[2,])
      v = piece[1,]
      # norm as max
      err = max(abs(v - f))
      #print(err)
      if (err <= mini){
        index1 = nPoints
        index2 = nInt
        mini = err
      }
    }
  }
  return(c(index1, index2, mini))
}

errMinPiece = function(budget,a,b,neval = 10^4,FUN){
  ##  Find the set of parameters that minimizes the error depending of the type of nodes
  ##' @param budget : the number of evaluations allowed for interpolation
  ##' @param a, b : interval end points 
  ##' @param neval : the max number of evaluations in total
  ##' @param FUN : the function of reference
  ##' @return: the lowest order that minimizes the interpolation error and the number of pieces.
  
  ## Determine minimal error for both Chebyshev and equidistant piecewise interpolation   
  errMinEqui = errMinParam(budget,a,b,neval,"equi",FUN)
  errMinCheby = errMinParam(budget,a,b,neval,"cheby",FUN)
  if (errMinCheby[3] <= errMinEqui[3]){
    errMin = errMinCheby
    print("Chebyshev nodes")
  } else {
    errMin = errMinEqui
    print("Equidistant nodes")
  }
  return(paste("Number of nodes per intervals= ",toString(errMin[1]), ", Number of intervals= ", toString(errMin[2]), ", Error= ", toString(errMin[3])))
}




##################################
######## Partie 2 ##############
######## Méthodes de quadrature ####

trapezeInt =function(FUN,a,b,M){
  ##' TRAPEZOIDAL INTEGRATION RULE (COMPOSITE)
  ##' @param FUN : the function to be integrated
  ##' @param a, b : interval end points 
  ##' @param M : number of intervals (each of size (b-a)/M)
  ##' @return: the value of the composite trapezoidal quadrature. 
  x = seq(a,b, length.out= M+1)
  y = sapply(x, FUN)
  h = (b-a)/M
  v = rep(1,M+1)
  v[1] = 1/2 ; v[M+1] = 1/2
  q = h*sum(v*y)
  
  return(q)
}

refineTrapeze=function(FUN,a,b,M,q){
  ##' refinement of the subdivision step: incremental method
  ##' @param FUN : the function to be integrated
  ##' @param a, b : interval end points 
  ##' @param M : initial number of intervals (each of size (b-a)/M)
  ##'  having been used to compute q
  ##' @param  q : the value of the trapezoidal  quadrature method
  ##'  of stepsize (b-a)/M
  ##' @return : the value of the quadrature for a stepsize h' = h/2
  h = (b-a)/M
  x =  a + (2*seq(0,M-1, length.out= M) + 1)*h/2
  y = sapply(x, FUN)
  v = rep(1,M)
  Q = 0.5*q + h/2*sum(y*v)
  return(Q)
}

simpsonInt = function(FUN,a,b,M){
  ##' Simpson integration via trapeze rule
  ##' uses the fact that 
  ##' simpson(h) = 4/3(trapeze(h/2) - 1/4 trapeze(h))
  ##' @param FUN : the function to be integrated
  ##' @param a, b : interval end points 
  ##' @param M : initial number of intervals (each of size (b-a)/M)
  ##' @return : the value of the quadrature for a stepsize h
  h = (b-a)/M;
  qtrapeze = trapezeInt(FUN,a,b,M)
  qrefined = refineTrapeze(FUN,a,b,M,qtrapeze)
  q = 4/3 * (qrefined - qtrapeze/4)
  return(q)
}

evalErrSimpson = function(FUN,a,b,M){
  ## Computes an approximation E of the error(2M)
  ## for the composite Simpson rule of step h=(b-a)/(2M). 
  ##This requires computing I_M and I_{2M}. 
  ##The value  q = I_{2M} is also returned. 
  ##' @param FUN : the function to be integrated
  ##' @param a, b : interval end points 
  ##' @param M : initial number of intervals (each of size (b-a)/M)
  ##' @return : an approximation of the quadrature error 
  qth = trapezeInt(FUN,a,b,M)   ## M +1 evaluations
  qth2 = refineTrapeze (FUN,a,b,M,qth)  ## M evaluations
  qth4 = refineTrapeze (FUN,a,b,2*M,qth2)   ## 2M evaluations
  simps_h =   4/3*(qth2 - 1/4* qth) 
  simps_h2 =  4/3*(qth4 - 1/4*qth2) 
  q = simps_h2  
  E = (simps_h - simps_h2)/15 
  return(c(E,q))
}

precisionMSimspon = function(FUN,a,b,init,precision) {
  ## Determine a approximation of optimal of optimal M for simpson quadrature 
  ##' @param FUN : the function to be integrated
  ##' @param a, b : interval end points 
  ##' @param M : initial number of intervals (each of size (b-a)/M)
  ##'  having been used to compute q
  ##' @param  q : the value of the trapezoidal  quadrature method
  ##'  of stepsize (b-a)/M
  ##' @return : the value of M that minimizes the approximated quadrature error
  M = init
  thres = precision / 10
  err = abs(evalErrSimpson(FUN,a,b,M)[1])
  while (err > thres) {
    M = 2*M
    err = abs(evalErrSimpson(FUN,a,b,M)[1])
    #print(err)
  }
  return(M)
}
  

##################################
######## Partie 3 ##############
######## Extrapolation de Richardson ####

richardson = function(FUN,n,t,delta,start, Plot){
  ## Compute Newton divided differences of interpolated polynomial on nodes
  ## t, t\delta, ... , t\delta^n that gives function value at start
  ##' @param FUN : the function to be integrated
  ##' @param n : max power for nodes 
  ##' @param t : scale parameter
  ##' @param delta : factor of decrease
  ##' @param start : point where the function is to be evaluated
  ##' @param Plot : boolean to indicate if plot comparison Naive / Richardson 
  ##' @return : an array of successive approximation of FUN(0), the last one is the best (A[n+1])
  ##'
  lx = log(t)  +  log(delta) *(0:n)
  x = start + exp(lx) 
  A = sapply(x,FUN)
  y = A
  for( j in 2:(n+1)){
    A[j : (n+1) ] =  (A[j:(n+1)] - delta^j * A[(j-1):n] )/(1 - delta^j)
  }
  if(Plot){
    plot(0:n, y, ylab= paste("Value at node k for t,delta= ", toString(t), ",",toString(delta)), col='blue', type = "p")
    title(main = "Comparison Naive estimation / Richarson extrapolation ")
    lines( 0:n,A,col='red', type = "p")
    legend('topright', legend=c('Naive estimator ', 'Richardson extrapolation at 1'),
           col=c('blue', 'red'), pch = 19)
  }
  return(A)
}