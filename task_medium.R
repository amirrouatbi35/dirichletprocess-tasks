
install.packages("mixR")
library(mixR)
library(dplyr)
library(tidyr)
require(dirichletprocess)
library(ggplot2)
set.seed(1)
n <- 1000
data <- rmixlnorm(n = 200, pi=c(0.8,0.2), mu = c(1,2), sd=c(0.5,0.6))

data = scale(data)

hist(data, breaks = 30, col = "lightblue", xlab = "x", main = "Lognormal mixture model")

dp <- DirichletProcessGaussian(data)
dp <- Fit(dp, 1000)

plot(dp,data_method="hist")

postF <- PosteriorFunction(dp)
points <- seq(from = 0, to = 1, by = 0.1)
postSample <- replicate(100, postF(points))
plot(postSample)

priorF <- dirichletprocess:::PriorFunction(dp)
plot(priorF)
quantile(postSample, c(0.05,0.95))

meanFrame <- data.frame(Mean=rowMeans(postSample), x=points)
quantileFrame <- data.frame(x=points, t(apply(postSample, 1, quantile, prob=c(0.05, 0.95))))
ggplot()  + geom_ribbon(data=quantileFrame, aes(x=x, ymin=X5., ymax=X95.), alpha=0.5) + 
  geom_line(data=meanFrame, aes(x=x, y=Mean, colour="Posterior Mean")) 


require(palmerpenguins)
penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>% 
  scale -> trainData

data <- scale(trainData)
data <- na.omit(data)

dp1 <- DirichletProcessMvnormal(data, alphaPriors = c(2,4))
dp1 <- Fit(dp1, 100)
plot(dp1)

dp2 <- DirichletProcessMvnormal(data, alphaPriors = c(4,8))
dp2 <- Fit(dp2, 100)
plot(dp2)

plot(dp1$alphaChain)
plot(dp2$alphaChain)
df <- data.frame(x = 1:length(dp1$alphaChain),
                 y1 = dp1$alphaChain,
                 y2 = dp2$alphaChain)

ggplot(data = df) +
  geom_line(aes(x = x, y = y1,colour="alpha1")) +
  geom_line(aes(x = x, y = y2,colour="alpha2"))+
  scale_color_manual(values = c("alpha1" = "red", "alpha2" = "blue"))

