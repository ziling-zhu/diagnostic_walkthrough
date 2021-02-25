# Install: 
# - ggfortify
# - lawstat
# - lmtest
# - DataCombine
# - gvlma

# Load libs
library(tidyverse)
library("lawstat")
library("ggfortify")
library("lmtest")
library("DataCombine")
library("gvlma")


# Fit some models
mod1 <- lm(mpg ~ wt, data = mtcars)
mod2 <- lm(dist ~ speed, data = cars[1:20, ])
summary(mod1)
summary(mod2)

# Assumptions

# 1.
# The regression model is linear in parameters
# Eyeball it

mtcars %>% 
  ggplot(., aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method = lm)

cars[1:20, ] %>% 
  ggplot(., aes(x = speed, y = dist)) +
  geom_point() +
  geom_smooth(method = lm)








# 2. 
# The mean of residuals is zero
# How to check?: Check model summary and test manually
mean(mod1$residuals)
summary(mod1$residuals)
summary(mod2$residuals)

plot(mod2)
#blob, horizontal line
#qq plot
#scale location
#cook's distance & leverage

ds4ling::gg_qqplot(mod1$residuals)




# 3.
# Homoscedasticity of residuals or equal variance
# How to check? 
autoplot(mod1, which = c(1, 3))

# What are you looking for? The line should be more or less flat
autoplot(mod2, which = c(1, 3))












# 4.
# No autocorrelation of residuals (important for time series data)
# When the residuals are autocorrelated, it means that the current value 
# is dependent of the previous values and that there is an unexplained 
# pattern in the Y variable that shows up

#
# How to check? 3 methods
#

# 4a. afc plot
acf(mod1$residuals)   # visual inspection
data(economics)       # bad example
bad_auto <- lm(pce ~ pop, data = economics)
acf(bad_auto$residuals)  # highly autocorrelated from the picture.

# 4b. Runs test
lawstat::runs.test(mod1$residuals)
lawstat::runs.test(bad_auto$residuals)

# 4c. Durbin-Watson test
lmtest::dwtest(mod1)
lmtest::dwtest(bad_auto)

#
# How to fix it?
#

# One option: Add lag1 as predictor and refit model
econ_data  <- data.frame(economics, resid_bad_auto = bad_auto$residuals)
#lag every row down one row
econ_data1 <- slide(econ_data, Var = "resid_bad_auto", NewVar = "lag1", slideBy = -1)
econ_data2 <- na.omit(econ_data1)
bad_auto2  <- lm(pce ~ pop + lag1, data = econ_data2)

acf(bad_auto2$residuals)
lawstat::runs.test(bad_auto2$residuals)
lmtest::dwtest(bad_auto2)
summary(bad_auto2)

#
# What happened? Adding the lag variable removes the autocorrelation so now 
# we can interpret the parameter of interest.
#
# (you might never do this)
#










# 5. predictor and residuals are not correlated
# How to check? cor.test
cor.test(mtcars$wt, mod1$residuals)















# 6. 
# Normality of residuals
# (increasingly bad)
autoplot(mod1, which = 2)
autoplot(mod2, which = 2)
autoplot(bad_auto, which = 2)
#seems to be normally distributed








#
# You can check some assumptions automatically
#
gvlma::gvlma(mod1)
gvlma::gvlma(mod2)
gvlma::gvlma(bad_auto)











# 0. Create project 'diagnostics'
# 1. add folders "slides", "scripts", "data"
# 2. save mtcars to "data" (write_csv)
# 3. load "mtcars" (read_csv)
# 4. walk through diagnostics in Rscript 
# 5. create ioslides
#   - sections
#   - lists
#   - bold, italics
#   - r chunkcs
# 6. add diagnostics to slides 
# 7. convert to slidify 
# 8. create repo, push, github pages




# install xaringan

---
title: "Presentation Ninja"
subtitle: "⚔<br/>with xaringan"
author: "Yihui Xie"
institute: "RStudio, PBC"
date: "2016/12/12 (updated: `r Sys.Date()`)"
output:
  xaringan::moon_reader:
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---
