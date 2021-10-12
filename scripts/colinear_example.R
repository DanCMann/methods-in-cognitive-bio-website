#' Script to demonstrate how colinearity can affect the interpretation of 
#'  individual predictors. 

library(ggplot2)

## Set seed for reproducibility
set.seed(42)

## sample size
ss <- 100

## random heights
h <- rnorm(ss, 170, sd = 10)
## legs as a proportion of heights, uniform variation from .45 to .5
lg_prop <- runif(ss, 0.45, 0.5)
## create the leg length variable (with a bit of error)
lg <- h * lg_prop  + rnorm(ss, 0, .01)
## shoe size proportion 
sh_prop <- runif(ss, 0.09, 0.1)
## make shoe size as a proportion of leg length to reinforce colinearity. 
sh <- lg * sh_prop + rnorm(ss, sd = 0.06)

df <- data.frame(height = h, leg_len = lg, shoe_size = sh)

## check correlations
cor.test(df$height, df$shoe_size)
plot(df$shoe_size, df$height)

plot(df$height, df$leg_len)
cor.test(df$height, df$leg_len)

## fit the model 
mod <- lm(height ~ leg_len + shoe_size, data = df)

summary(mod)

