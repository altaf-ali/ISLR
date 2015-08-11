#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
#
# EPage 138
# 
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }
library(MASS)
library(boot)

Boston = na.omit(Boston)

# Part (a):
#
mu_hat = mean( Boston$medv )

# Part (b):
#
n = dim(Boston)[1]
mu_se = sd( Boston$medv )/sqrt(n)

mean_boot.fn = function(data,index){
  mean( data[index] )
}

boot( Boston$medv, mean_boot.fn, 1000 )

# Part (e):
#
median( Boston$medv )

median_boot.fn = function(data,index){
  median( data[index] )
}

boot( Boston$medv, median_boot.fn, 1000 )

# Part (g):
#
quantile( Boston$medv, probs=c(0.1) )

ten_percent_boot.fn = function(data,index){
  quantile( data[index], probs=c(0.1) )
}

boot( Boston$medv, ten_percent_boot.fn, 1000 )

  

