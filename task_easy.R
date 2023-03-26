
devtools::install_github("dm13450/dirichletprocess")
require(dirichletprocess)
library(magrittr)
library(dplyr)
library(ggplot2)

#scaling data
faithfulTrans <- (faithful$waiting - mean(faithful$waiting))/sd(faithful$waiting)

dp <- DirichletProcessGaussian(faithfulTrans)

#fitting
dp <- Fit(dp, 100)
plot(dp)

install.packages("palmerpenguins")
require(palmerpenguins)



penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>% 
  scale -> trainData

#omitting N/A values
trainData <- na.omit(trainData)


dp <- DirichletProcessMvnormal(trainData)
dp <- Fit(dp, 100)
plot(dp)



