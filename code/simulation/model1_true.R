# NYU Greene

.libPaths("/burg/home/yt2661/R/x86_64-pc-linux-gnu-library/4.1/")


library(dfoptim)
library(nnet)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(npcs)
library(naivebayes)
library(MASS)
library(conflicted)
library(stringr)
library(mvtnorm)

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename = paste("/burg/home/yt2661/projects/NPMC/experiment/simulation/model1_true/result/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}


# -----------------------------------------------------------------------
f <- function(alpha1) {
  pnorm(-sqrt(sum((c(0,1,0,1,0) -c(-1,2,-1,1,1))^2))-qnorm(alpha1))
}

mu1 <- c(-1,2,-1,1,1)
mu3 <- c(1,1,-1,0,1)
mu2 <- c(0,1,0,1,0)
Sigma3 <- matrix(c(norm(mu1-mu3, "2")^2, sum((mu1-mu3)*(mu2-mu3)),
                   sum((mu1-mu3)*(mu2-mu3)), norm(mu2-mu3, "2")^2), nrow = 2)
Sigma1 <- matrix(c(norm(mu2-mu1, "2")^2, sum((mu2-mu1)*(mu3-mu1)),
                   sum((mu2-mu1)*(mu3-mu1)), norm(mu3-mu1, "2")^2), nrow = 2)
Sigma2 <- matrix(c(norm(mu1-mu2, "2")^2, sum((mu3-mu2)*(mu1-mu2)),
                   sum((mu3-mu2)*(mu1-mu2)), norm(mu3-mu2, "2")^2), nrow = 2)

Er3 <- function(lambda) {
  1-pmvnorm(upper = c(0.5*norm(mu1-mu3, "2")^2 - log(lambda[1]),
                      0.5*norm(mu2-mu3, "2")^2 - log(lambda[2])), sigma = Sigma3)
}

Er1 <- function(lambda) {
  1-pmvnorm(upper = c(0.5*norm(mu2-mu1, "2")^2 - log(lambda[2]) + log(lambda[1]),
                      0.5*norm(mu3-mu1, "2")^2 + log(lambda[1])), sigma = Sigma1)
}

Er2 <- function(lambda) {
  1-pmvnorm(upper = c(0.5*norm(mu1-mu2, "2")^2 - log(lambda[1]) + log(lambda[2]),
                      0.5*norm(mu3-mu2, "2")^2 + log(lambda[2])), sigma = Sigma2)
}


G <- function(lambda, alpha) {
  Er3(lambda) + lambda[1]*(Er1(lambda) - alpha[1]) + lambda[2]*(Er2(lambda) - alpha[2])
}

obj_function <- function(alpha){
  hjkb(par = rep(0.0001, 2), fn = G, upper = rep(1000, 2), lower = rep(0.000001, 2), control = c(maximize = TRUE), alpha = alpha)$value
}


alpha1 <- seed/1000
obj_value <- sapply(1:1000, function(i){
  alpha2 <- i/1000
  if (alpha2 < f(alpha1)) {
    NA
  } else {
    obj_function(c(alpha1, alpha2))
  }
})


save(obj_value, file = filename)

