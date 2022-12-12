# One-way cluster function (Arai 2015)
clx <-
  function(fm, dfcw, cluster){
    library(sandwich)
    library(lmtest)
    M <- length(unique(cluster))
    N <- length(cluster)
    dfc <- (M/(M-1))*((N-1)/(N-fm$rank))
    u <- apply(estfun(fm),2,
               function(x) tapply(x, cluster, sum))
    vcovCL <- dfc*sandwich(fm, meat=crossprod(u)/N)*dfcw
    coeftest(fm, vcovCL) }


# Two-way cluster function (Arai 2015)
mclx <-
  function(fm, dfcw, cluster1, cluster2){
    library(sandwich)
    library(lmtest)
    cluster12 = paste(cluster1,cluster2, sep="")
    M1 <- length(unique(cluster1))
    M2 <- length(unique(cluster2))
    M12 <- length(unique(cluster12))
    N <- length(cluster1)
    K <- fm$rank
    dfc1 <- (M1/(M1-1))*((N-1)/(N-K))
    dfc2 <- (M2/(M2-1))*((N-1)/(N-K))
    dfc12 <- (M12/(M12-1))*((N-1)/(N-K))
    u1 <- apply(estfun(fm), 2,
                function(x) tapply(x, cluster1, sum))
    u2 <- apply(estfun(fm), 2,
                function(x) tapply(x, cluster2, sum))
    u12 <- apply(estfun(fm), 2,
                 function(x) tapply(x, cluster12, sum))
    vc1 <- dfc1*sandwich(fm, meat=crossprod(u1)/N )
    vc2 <- dfc2*sandwich(fm, meat=crossprod(u2)/N )
    vc12 <- dfc12*sandwich(fm, meat=crossprod(u12)/N)
    vcovMCL <- (vc1 + vc2 - vc12)*dfcw
    coeftest(fm, vcovMCL)}