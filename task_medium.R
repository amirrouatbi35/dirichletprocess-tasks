
install.packages("mixR")
library(mixR)
require(dirichletprocess)

set.seed(123)
n <- 1000
data <- rmixlnorm(n = 200, pi=c(0.8,0.2), mu = c(1,2), sd=c(0.5,0.6))

data = scale(data)

hist(data, breaks = 30, col = "lightblue", xlab = "x", main = "Lognormal mixture model")

dp <- DirichletProcessGaussian(data)
dp <- Fit(dp, 1000)

plot(dp)

postF = PosteriorFunction(dp)
plot(postF)

priorF = dirichletprocess:::PriorFunction(dp)
plot(priorF)

postF
