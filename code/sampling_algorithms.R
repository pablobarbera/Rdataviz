################################################################################
######## SIMULATING DATA #######################################################
################################################################################

## function to simulate IRT data
simulate.irt <- function(n=435, m=100){
    # n = number of legislators
    # m = number of bills
    require(MASS)

    # simulating latent variable
    theta <- rnorm(n=n, mean=0, sd=1)
    # simulating item parameters
    item.params <- mvrnorm(n=m, mu=c(0,0), Sigma=1*diag(2))

    # creating predicted probabilities
    ystar <- matrix(NA, nrow=n, ncol=m)
    for (i in 1:n){
        for (j in 1:m){
            ystar[i,j] <- item.params[j,1] + item.params[j,2] * theta[i] + 
                            + rnorm(n=1)
        }
    }
    # creating roll call records
    y <- matrix(NA, nrow=n, ncol=m)
    y[ystar>=0] <- 1
    y[ystar<0] <- 0
    return(list(y=y, theta=theta, alpha=item.params[,1], beta=item.params[,2]))
}

################################################################################
######## GIBBS SAMPLER #########################################################
################################################################################

## data augmentation step
augment.ystar <- function(theta, item.params, y, delta, S){
    require(msm)
    # Calculating means
    ystar <- matrix(t(item.params[,1]), nrow=nrow(y), ncol=ncol(y), byrow=TRUE) + 
            theta %*% t(item.params[,2])
    # Checking which ones are equal to one
    condition <- (y==1)
    # Updating ystar matrix
    ystar[condition] <- rtnorm(n=length(ystar[condition]), 
                                mean=ystar[condition], lower=0)
    ystar[!condition] <- rtnorm(n=length(ystar[!condition]), 
                                mean=ystar[!condition], upper=0)
    return(ystar) #returns ystar
}

## Function to sample from bivariate normal (faster than rmvnorm)
rbinorm <- function(mean, Sigma){
    sigma.chol <- chol(Sigma)
    z <- rnorm(2)
    y <- mean + t(sigma.chol) %*% z
    return(y)
}


## Sampling item parameters (updating b)
update.b <- function(theta, ystar, Tzero){
    Xstar <- cbind(1, theta)
    variance <- solve(t(Xstar) %*% Xstar + solve(Tzero))
    mean.vector <- variance %*% (t(Xstar) %*% ystar)
    item.params <- t(apply(mean.vector, 2, rbinorm, Sigma=variance))
    return(item.params)
}

## Sampling legislator parameters (updating theta)
update.theta <- function(item.params, ystar){
    w <- ystar - matrix(t(item.params[,1]), nrow=nrow(ystar), ncol=ncol(ystar), 
                byrow=TRUE)
    variance <- solve(t(item.params[,2]) %*% item.params[,2] + 1)
    mean.theta <- variance %*% (t(item.params[,2]) %*% t(w))
    theta <- apply(mean.theta, 2, rnorm, n=1, sd=sqrt(variance))
    return(theta)
}

normalize.item.params <- function(item.params, theta){
    item.params[,1] <- item.params[,1] + item.params[,2] * mean(theta)
    item.params[,1] <- (item.params[,1] - mean(item.params[,1]))
    item.params[,2] <- item.params[,2] * sd(theta)
    item.params[,2] <- (item.params[,2] - mean(item.params[,2]))
    return(item.params)
}

normalize.theta <- function(theta){
    theta <- (theta - mean(theta))/sd(theta)
    return(theta)
}


gibbs.sampler <- function(y, priors, start.theta=rnorm(dim(y)[1], 0, 1), 
    n.iter=1000, seed)
{
    require(MASS)
    set.seed(seed)
    # priors
    tzero <- priors$tzero; Tzero <- priors$Tzero
    Pzero <- priors$Pzero; pzero <- priors$pzero
    # starting values
    n <- dim(y)[1]; m <- dim(y)[2]
    message("n = ", n, " legislators and m = ", m, " bills")
    item.params <- mvrnorm(n=m, mu=c(0,0), Sigma=diag(2))
    theta <- as.numeric(start.theta)
    # emtpy arrays to save results
    items <- array(NA, dim=c(n.iter, m, 2))
    thetas <- matrix(NA, nrow=n.iter, ncol=n)
    # initializing Gibbs sampler
    iter <- 1
    message("Initializing Gibbs sampler: ", n.iter, " iterations")
    pb <- txtProgressBar(min=1,max=n.iter, style=3)
    # gibbs sampler:
    while (iter <= n.iter){
        # Sampling
        ystar <- augment.ystar(theta, item.params, y)
        item.params <- update.b(theta, ystar, Tzero)
        theta <- update.theta(item.params, ystar)
        # normalization
        theta <- normalize.theta(theta)
        item.params <- normalize.item.params(item.params, theta)
        # Saving results
        items[iter,,] <- item.params
        thetas[iter,] <- theta
        # Next iteration
        setTxtProgressBar(pb, iter)
        iter <- iter + 1
    }

    return(list(items=items, theta=thetas))
}


################################################################################
######## METROPOLIS ALGORITHM ##################################################
################################################################################

# log posterior density function
lpd <- function(alpha, beta, theta, y){
    value <- alpha + beta * theta
    # likelihood
    sum( y * pnorm(value, log=TRUE) + (1-y) * pnorm(-value, log=TRUE)) +
    # priors
    sum(dnorm(theta, 0, 1, log=TRUE)) + 
    sum(dnorm(beta, mean=0, sd=25, log=TRUE)) +
    sum(dnorm(alpha, 0, 25, log=TRUE))
}

# metropolis algorithm function
metropolis.irt <- function(y, start.theta=rnorm(dim(y)[1], 0, 1),
    n.iter=1000, delta=0.05, seed)
{
    set.seed(seed)
    # empty arrays to save results
    n <- dim(y)[1]; m <- dim(y)[2]
    alphas <- matrix(NA, nrow=n.iter, ncol=m)
    betas <- matrix(NA, nrow=n.iter, ncol=m)
    thetas <- matrix(NA, nrow=n.iter, ncol=n)
    # initial values
    alpha.cur <- rnorm(n=m, 0, 2)
    beta.cur <- rnorm(n=m, 0, 2)
    theta.cur <- start.theta

    # initializing sampler
    iter <- 1
    pb <- txtProgressBar(min=1,max=n.iter, style=3)
    while (iter <= n.iter){
        # sampling candidate values
        alpha.cand <- sapply(alpha.cur, function(x) 
            runif(n=1, min=x-delta, max=x+delta))
        beta.cand <- sapply(beta.cur, function(x) 
            runif(n=1, min=x-delta, max=x+delta))
        theta.cand <- sapply(theta.cur, function(x) 
            runif(n=1, min=x-delta, max=x+delta))
        # computing acceptance probability
        accept.prob <- exp(lpd(alpha.cand, beta.cand, theta.cand, y) - 
            lpd(alpha.cur, beta.cur, theta.cur, y))
        alpha <- min(accept.prob, 1)
        # jumping with probability alpha
        if (runif(1)<=alpha) { 
            alpha.cur <- alpha.cand
            beta.cur <- beta.cand
            theta.cur <- theta.cand
        }
        alphas[iter,] <- alpha.cur
        betas[iter,] <- beta.cur
        thetas[iter,] <- theta.cur
        iter <- iter + 1
        setTxtProgressBar(pb, iter)
    }
    return(list(theta=thetas, alpha=alphas, beta=betas))
}


################################################################################
######## HAMILTONIAN MONTE-CARLO ###############################################
################################################################################


stan.code <- '
data {
  int<lower=1> J; // number of legislators
  int<lower=1> K; // number of bills
  int<lower=1> N; // number of observations
  int<lower=1,upper=J> jj[N]; // legislator for observation n
  int<lower=1,upper=K> kk[N]; // bill for observation n
  int<lower=0,upper=1> y[N]; // vote of observation n
}
parameters {
  real alpha[K];             
  real beta[K];   
  real theta[J];
}
model {
  alpha ~ normal(0,25);
  beta ~ normal(0,25);
  theta ~ normal(0,1);
  for (n in 1:N)
    y[n] ~ bernoulli(Phi(alpha[kk[n]] + beta[kk[n]] * theta[jj[n]]));
}
'










