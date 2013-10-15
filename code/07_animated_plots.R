###############################################################################
## Using ggplot2 to generate animated plots
## Author: Pablo Barberá
## Data Visualization with R and ggplot2
## October 15th 2013
###############################################################################

## Using animated plots to compare different Bayesian sampling algorithms

## 'sampling_algorithms.R' contains code to fit an IRT model using a Gibbs
## sampler with a data-augmentation step, a Metropolis-Hastings algorithm,
## and a Hamiltonian Monte-Carlo algorithm (implemented in Stan)
source("code/sampling_algorithms.R")

# SIMULATING DATA
# The following code generates simulated roll-call votes for J legislators
# and K bills assuming the standard 3-parameter item-response model,
# where theta is ideology of legislators, alpha is item difficulty, and beta
# is item discrimination (see e.g. Gelman and Hill, 2007, pp. 314-321)
set.seed(12345)
J <- 500; K <- 50; N <- J * K
jj <- rep(1:J, times=K)
kk <- rep(1:K, each=J) 
irt.sim <- simulate.irt(n=J, m=K)
y <- irt.sim$y

# Non-informative priors
priors <- list(tzero = c(0, 0),
                Tzero = 25 * diag(2),
                Pzero = 25,
                pzero = 0)


## gibbs sampler
gibbs.results <- gibbs.sampler(y, priors, n.iter=300, seed=123)

## metropolis-hastings
metropolis.results <- metropolis.irt(y, n.iter=300, seed=999, delta=0.15)

## hamiltonian monte-carlo
stan.data <- list(J=J, K=K, N=N, jj=jj, kk=kk, y=c(y))
inits <- list(list(alpha = rnorm(K, 0, 1), beta = rnorm(K, 0, 1),
                theta = rnorm(J, 0, 1)))
library(rstan)
stan.model <- stan(model_code=stan.code, data = stan.data, iter=1, 
    warmup=0, chains=1, seed=999) # compiling model
stan.fit <- stan(fit=stan.model, data=stan.data, iter=400, 
    warmup=100, chains=1, thin=1, init=inits, seed=999) # running model
stan.results <- extract(stan.fit) # samples from posterior

## loading from backup
load("backup/gibbs.results")
load("backup/metropolis.results")
load("backup/stan.results")

## preparing data for plot
gibbs <- data.frame(
        beta = gibbs.results$items[,1,2],
        theta = gibbs.results$theta[,1],
        theta.end = c(gibbs.results$theta[-1,1], NA),
        beta.end = c(gibbs.results$items[-1,1,2], NA),
        samp = "Gibbs"
    )
metr <- data.frame(
        beta = metropolis.results$beta[,1],
        theta = -metropolis.results$theta[,1],
        theta.end = c(-metropolis.results$theta[-1,1], NA),
        beta.end = c(metropolis.results$beta[-1,1], NA),
        samp = "Metropolis"
    )
hmc <- data.frame(
        beta = -stan.results$beta[,1],
        theta = -stan.results$theta[,1],
        theta.end = c(-stan.results$theta[-1,1], NA),
        beta.end = c(-stan.results$beta[-1,1], NA),
        samp = "HMC"
    )
d <- rbind(gibbs, metr, hmc)


## first, we plot distribution of estimates to get axes limits
library(ggplot2)
p <- ggplot(d, aes(x=theta, y=beta))
pq <- p + geom_point(color="black") + facet_grid(~samp) + theme_bw() +
    theme(panel.grid = element_blank())
pq

## we can compare with the true estimates
pq <- pq +
    geom_vline(xintercept = irt.sim$theta[1], linetype=3) +
    geom_hline(yintercept = irt.sim$beta[1], linetype=3)
pq

## now, we create an empty plot
p <- ggplot(d, aes(x=theta, y=beta))
pq <- p +  ## fixing limits of axes to min/max of data
    scale_x_continuous(expression(theta[1]), lim=c(min(d$theta), max(d$theta))) + 
    scale_y_continuous(expression(beta[1]), lim=c(min(d$beta), max(d$beta))) + 
    # note that we can get greek letter using the "expression" function
    geom_vline(xintercept = irt.sim$theta[1], linetype=3) +
    geom_hline(yintercept = irt.sim$beta[1], linetype=3) +
    facet_grid(~samp) + theme_bw() + 
    theme(panel.grid = element_blank())
pq

## this will be the first plot, with just the starting values in the chain
pq0 <- pq + ggtitle("Comparing Sampling Algorithms. Starting values.") +
    geom_point(data = gibbs[1,], aes(x=theta, y=beta)) +
    geom_point(data = metr[1,], aes(x=theta, y=beta)) +
    geom_point(data = hmc[1,], aes(x=theta, y=beta))
pq0
ggsave(pq0, file="plots/bayes/bayes1.pdf", height=4, width=7)

## and now we add lines that join the values in each iteration
i <- 1
pq <- pq + geom_segment(data = gibbs[i,], 
            aes(x=theta, y=beta, xend=theta, yend=beta.end)) +
        geom_segment(data = gibbs[i,], 
            aes(x=theta, y=beta.end, xend=theta.end, yend=beta.end)) +
        geom_segment(data = metr[i,],
            aes(x=theta, y=beta, xend=theta.end, yend=beta.end)) +
        geom_segment(data = hmc[i,],
            aes(x=theta, y=beta, xend=theta.end, yend=beta.end)) +
        ggtitle(paste("Comparing Sampling Algorithms. Iteration", i))
pq
ggsave(pq, file=paste0("plots/bayes/bayes", i+1, ".pdf"), height=4, width=7)

## and we do the same for the first 100 iterations
for (i in 2:100){
    pq <- pq + geom_segment(data = gibbs[i,], 
            aes(x=theta, y=beta, xend=theta, yend=beta.end)) +
        geom_segment(data = gibbs[i,], 
            aes(x=theta, y=beta.end, xend=theta.end, yend=beta.end)) +
        geom_segment(data = metr[i,],
            aes(x=theta, y=beta, xend=theta.end, yend=beta.end)) +
        geom_segment(data = hmc[i,],
            aes(x=theta, y=beta, xend=theta.end, yend=beta.end)) +
        ggtitle(paste("Comparing Sampling Algorithms. Iteration", i))
    ggsave(pq, file=paste0("plots/bayes/bayes", i+1, ".pdf"), height=4, width=7)
}

## adding a background image 
library(gridExtra)
library(jpeg)
img <- readJPEG("plots/background.jpg")
g <- rasterGrob(img, interpolate=TRUE)
pqf <- pq + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

ggsave(pqf, file="plots/bayes/bayes102.pdf", height=4, width=7)

## saving into a video file (requires ImageMagick)
# first, convert all images to .png
for (i in 1:101){
    system(paste0("convert -density 300 -depth 8 -quality 100 "
        "plots/bayes/bayes", i, ".pdf plots/bayes/png/", sprintf("%03d", i), 
        ".png"))
}

# then turn images into video (requires ffmepg)
system(paste0("ffmpeg -r 3 -start_number 001 -i plots/bayes/png/%03d.png ",
    "-b 1600k plots/bayes.mp4"))
## -r = frames per second
## -b = bitrate

## creating a gif file
system(paste0("convert -delay 1 -loop 0 -resize 30% -quality 50% ",
    "plots/bayes/png/*.png plots/animation.gif"))






