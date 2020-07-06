CAR_cov_teeth <- function(m, rho, tau){
  ##-- Tests
  if(!is.numeric(m)) stop("m must be an integer number.")
  if(abs(rho) > 1) stop("rho must be a number in the (0, 1) interval.")
  if(!is.numeric(tau) | tau < 0) stop("tau must be a positive number.")
  
  if(m > 1){
    D_m <- abs(outer(1:m, 1:m, "-")) == 1
    M_m <- diag(colSums(D_m))
    Sigma <- tau^2*solve(M_m-rho*D_m)
  } else{
    Sigma <- tau^2
  }
  
  return(Sigma)
}


MC_var_yibar_mis <- function(mu, Sigma, sigma1, lambda, nu, sigma0, Num, a0, b0, cutoff){
  m <- dim(Sigma)[1]
  
  Qit <- rmvnorm(Num, rep(0, m), Sigma)
  Yit <- matrix(1, nrow = Num, ncol = 1)%*%matrix(mu, nrow = 1, ncol = m) + Qit + matrix(rst(n = Num*m, xi = 0, omega = sigma1, alpha = lambda, nu = nu), nrow = Num, ncol = m)
  
  Iit <- a0 + Qit*b0 + rmvnorm(Num, rep(0, m), diag(sigma0^2, m))
  Mit <- ifelse(Iit > cutoff, 1, 0)
  
  mYi <- mean(rowSums(Yit*(1 - Mit))/rowSums(1-Mit), na.rm = T)
  VarYi <- var(rowSums(Yit*(1 - Mit))/rowSums(1-Mit), na.rm = T)
  
  PM <- mean(rowSums(Mit)/m)
  
  return(list(Yit = Yit, Mit = Mit, Iit = Iit, mYi = mYi, VarYi = VarYi, PM = PM))
}

b0fun <- function(c_i, b0, Sigma, sigma1, nu, lambda, sigma0){
  if(nu < Inf){
    mean(b0*diag(Sigma)/sqrt((diag(Sigma) + (sigma1^2*nu/(nu - 2) - nu/pi*(gamma(0.5*(nu - 1))/gamma(0.5*nu))^2*sigma1^2*(lambda^2/(1 + lambda^2))))*(b0^2 * diag(Sigma) + sigma0^2))) - c_i
  } else{
    mean(b0*diag(Sigma)/sqrt((diag(Sigma) + (sigma1^2 - 2/pi*sigma1^2*(lambda^2/(1 + lambda^2))))*(b0^2 * diag(Sigma) + sigma0^2))) - c_i
  }
}

pifun <- function(cutoff, a0, b0, Sigma, sigma0){
  Epit <- rep(0, 28)
  for(j in 1:28){
    Epit[j] <- stats::pnorm((cutoff - a0)/sqrt(b0^2 * Sigma[j, j] + sigma0^2))
  }
  res <- mean(Epit)
  return(res)
}

a0fun <- function(p_i, cutoff, a0, b0, Sigma, sigma0){
  res <- pifun(cutoff, a0, b0, Sigma, sigma0) - p_i
  return(res)
}



SampleSize_SMARTp = function(mu, st1, dtr,
                             regime, pow,
                             a,
                             rho, tau,
                             sigma1, lambda, nu, sigma0,
                             Num, p_i, c_i,
                             a0, b0,
                             cutoff){
  
  if(missing(regime)) stop('regime need to be specified')
  if(missing(pow)) pow <- 0.8
  if(missing(a)) a <- 0.05
  if(missing(rho)) rho <- 0.975
  if(missing(tau)) tau <- 0.85
  if(missing(sigma1)) sigma1 <- 0.95
  if(missing(lambda)) lambda <- 0
  if(missing(nu)) nu <- Inf
  if(missing(sigma0)) sigma0 <- 1
  if(missing(Num)) Num <- 1000000
  if(missing(a0)) a0 <- -1
  if(missing(b0)) b0 <- 0.5
  if(missing(cutoff)) cutoff <- 0
  
  m <- dim(mu)[2]
  
  no.regime <- length(regime)
  if( no.regime > 2 ){
    z.a.by.2 <- qnorm(1-a/1)
  }
  else{
    z.a.by.2 <- qnorm(1-a/2)    
  }

  
  Sigma <- CAR_cov_teeth(m = m, rho = rho, tau = tau)
  
  if(missing(c_i)){
    b0 <- b0
  } else{
    b0 <- uniroot(b0fun, c(-1, 1), tol = 0.0001, c_i = c_i, Sigma = Sigma, sigma1 = sigma1, nu = nu, lambda = lambda, sigma0 = sigma0)$root
  }
  
  if(missing(p_i)){
    a0 <- a0
  } else{
    a0 <- uniroot(a0fun, c(-100, 100), tol = 0.0001, p_i = p_i, cutoff = cutoff, b0 = b0, Sigma = Sigma, sigma0 = sigma0)$root
  }
  
  initr <- as.matrix(rep(st1[, 4], st1[, 1] + st1[, 2]))
  ga <- as.matrix(rep(st1[, 3], st1[, 1] + st1[, 2]))
  res <- c()
  p_st2 <- c()
  
  for(i in 1:dim(st1)[1]){
    res <- c(res, rep(c(1, 0), st1[i,1:2]))
    p_st2 <- c(p_st2, rep(1/st1[i, 1:2], st1[i, 1:2]))
  }
  
  res <- as.matrix(res)
  p_st2 <- as.matrix(p_st2)
  
  ga_comp <- rep(0, no.regime)
  mu_comp <- array(0,c(2,m,no.regime))
  p2_comp <- matrix(0, 2, no.regime)
  p1_comp <- rep(0,no.regime)
  
  ga_comp <- ga[dtr[regime, 2],]
  
  for(j in 1:no.regime){
    mu_comp[, , j] <- rbind(mu[dtr[regime[j], 2],], mu[dtr[regime[j], 3],])
  }
  for(j in 1:no.regime){
    p2_comp[, j] <- rbind(p_st2[dtr[regime[j], 2], ], p_st2[dtr[regime[j], 3], ])
  }
  
  p_st1 <- as.matrix(rep((colSums(t(1/st1[, 1:2])*rbind(st1[, 3], (1-st1[, 3]))))^(-1)/sum((colSums(t(1/st1[, 1:2])*rbind(st1[, 3], (1-st1[, 3]))))^(-1)),
                         st1[, 1] + st1[, 2]))
  p1_comp <- p_st1[dtr[regime, 2], ]
  p1_comp <- as.vector(p1_comp)
  
  mud <- matrix(NA, no.regime, 2)
  Sigd <- matrix(NA, no.regime, 2)
  ybar <- rep(NA, no.regime)
  sig.dd <- matrix(NA, no.regime, no.regime)
  sig.e.sq <- matrix(NA, max(no.regime-1 , 1), max(no.regime-1, 1))
  
  for(j in 1:no.regime){
    YibardR <- MC_var_yibar_mis(mu = mu_comp[1, , j], Sigma = Sigma, sigma1 = sigma1,
                                lambda = lambda, nu = nu, sigma0 = sigma0, Num = Num, a0 = a0, b0 = b0, cutoff = cutoff)
    YibardNR <- MC_var_yibar_mis(mu = mu_comp[2, , j], Sigma = Sigma, sigma1 = sigma1,
                                 lambda = lambda, nu = nu, sigma0 = sigma0, Num = Num, a0 = a0, b0 = b0, cutoff = cutoff)
    
    Sigd[j, 1] <- YibardR$VarYi
    mud[j, 1] <- YibardR$mYi
    
    Sigd[j, 2] <- YibardNR$VarYi
    mud[j, 2] <- YibardNR$mYi
    
    ybar[j] <- mud[j, 1]*ga_comp[j] + mud[j, 2]*(1-ga_comp[j])
    
    vr <- (ga_comp[j]/(p1_comp[j]*p2_comp[1, j]))*((Sigd[j, 1]) + (1-p1_comp[j]*p2_comp[1, j])*(mud[j, 1])^2)
    vnr <- ((1-ga_comp[j])/(p1_comp[j]*p2_comp[2, j]))*((Sigd[j, 2]) + (1-p1_comp[j]*p2_comp[2, j])*(mud[j, 2])^2)
    vrnrdiff <- ga_comp[j]*(1-ga_comp[j])*(mud[j, 1] - mud[j, 2])^2
    sig.dd[j,j] <- vr+vnr+vrnrdiff;
  }
  
  if(no.regime == 1){
    Del <- abs(ybar[1])
  } else{
    Del <- abs((rep(ybar[1], no.regime) - ybar)[-1])
  }
  
  for(i in 1:max(no.regime-1, 1)){
    for( j in min(i+1, no.regime):(no.regime-0)){
      if(no.regime == 1){
        covr <- sig.dd[1, 1]
      } else{
        if(dtr[regime[i], 4] == dtr[regime[j], 4]){
          covr <- (ga_comp[i]/(p1_comp[i]*p2_comp[1, i]))*((Sigd[i, 1]) + (mud[i, 1])^2) - (ga_comp[i]*ga_comp[j])*(mud[i, 1]*mud[j, 1]) - (ga_comp[i]*(1-ga_comp[j]))*(mud[i, 1]*mud[j, 2]) - ((1-ga_comp[i])*ga_comp[j])*(mud[i, 2]*mud[j, 1]) - ((1-ga_comp[i])*(1-ga_comp[j]))*(mud[i, 2]*mud[j, 2])
        } else{
          if(dtr[regime[i], 4] != dtr[regime[j], 4]){
            covr <- -(ga_comp[i]*ga_comp[j])*(mud[i, 1]*mud[j, 1]) - (ga_comp[i]*(1-ga_comp[j]))*(mud[i, 1]*mud[j, 2]) - ((1-ga_comp[i])*ga_comp[j])*(mud[i, 2]*mud[j, 1]) - ((1-ga_comp[i])*(1-ga_comp[j]))*(mud[i, 2]*mud[j, 2])
          }
        }
      }
      sig.dd[i,j]=covr
    }
  }
  
  for(j in 1:max(no.regime-1, 1)){
    sig.e.sq[j, j] <- sig.dd[1, 1] + ifelse(no.regime == 1, 0, sig.dd[j+1, j+1]) - 2*ifelse(no.regime == 1, 0, sig.dd[1, j+1])
  }
  
  for(i in 1:max(no.regime-2, 1)){
    for(j in ifelse(no.regime <= 2, 1, i+1):max(no.regime-1, 1)){
      sig.e.sq[i, j] <- sig.dd[1, 1] - ifelse(no.regime == 1, 0, sig.dd[1, (i+1)]) - ifelse(no.regime == 1, 0, sig.dd[1, (j+1)]) + ifelse(no.regime == 1, 0, sig.dd[(i+1), (j+1)] )
    }
  }
  
  sig.e.sq[lower.tri(sig.e.sq, diag = FALSE)] <- t(sig.e.sq)[lower.tri(sig.e.sq, diag = FALSE)]
  diag(sig.e.sq) <- diag(sig.e.sq) + 0.001
  
  sig.e.sq.cor <- cov2cor(sig.e.sq)
  Del_std <- Del/sqrt(diag(sig.e.sq)/2)
  ssize.fct <- function(N, Del_std, z.a.by.2, sig.e.sq.cor, pow){
    crit.vals <- z.a.by.2 - sqrt(N/2)*Del_std
    pmvnorm(upper = -as.vector(crit.vals), sigma = as.matrix(sig.e.sq.cor)) - pow
  }
  
  N <- uniroot(ssize.fct, interval = c(2, 1000), extendInt = "yes", Del_std = Del_std, z.a.by.2 = z.a.by.2, sig.e.sq.cor = sig.e.sq.cor, pow = pow)$root
  
  out <- list(N = N,
              sig.dd = sig.dd,
              sig.e.sq = sig.e.sq,
              Del = Del,
              Del_std = Del_std,
              ybar = ybar,
              initr = initr,
              ga = ga,
              res = res,
              p_st2 = p_st2,
              p_st1 = p_st1,
              Sigma = Sigma
  )
  
  class(out) <- "SMARTp"
  
  return(out)
}
