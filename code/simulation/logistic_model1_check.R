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

conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")

Sys.setenv(LANG = "en_US.UTF-8")
seed <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cat("seed=", seed, "\n")

filename = paste("/burg/home/yt2661/projects/NPMC/experiment/simulation/logistic_model1_check/result/", seed, ".RData", sep = "")
if (file.exists(filename)) {
  stop("Done!")
}

set.seed(0, kind = "L'Ecuyer-CMRG")


# -----------------------------------------------------------------------
train.set <- generate_data(n=1e5, model.no = 3)
x <- train.set$x
y <- train.set$y


# fit multinomial regression using all data
fit_multinomial <- data.frame(x = x, y = y) %>% multinom(y~., data = ., maxit = 200, trace = FALSE)


# NPMC
w <- c(0, 0, 1)
alpha1 <- seed/100
alpha <- c(alpha1, 0, NA)
D_matrix <- matrix(nrow = 100, ncol = 3, dimnames = list((1:100)/100, c("s", "f", "obj.value")))
for (i in 1:100) {
  print(i)
  alpha[2] <- i/100
  fit.npmc.CX <- try(npcs(x, y, algorithm = "CX", classifier = "logistic", w = w, alpha = alpha, limit = 1000,
                          fitted.model = fit_multinomial))
  D <- duality_check(fit.npmc.CX, x, y, delta = 0.1, R.G = 10)
  D_matrix[i, 1:2] <- D
  if (D["f"] == 1) {
    fit.npmc.CX$obj.value <- min(fit.npmc.CX$obj.value, 1+0.1)
  }
  D_matrix[i, 3] <- fit.npmc.CX$obj.value
}





save(D_matrix, file = filename)

