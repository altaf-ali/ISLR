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
library(boot)

set.seed(0)

Default = na.omit(Default)

m0 = glm( default ~ income + balance, data=Default, family="binomial" )
summary(m0)

validation_error_est = function(){
  #
  # Predictors are income and balance
  #
  
  # (i):
  # 
  n = dim(Default)[1]
  training_samples = sample(1:n,floor(n/2))
  validation_samples = (1:n)[-training_samples] 

  # (ii):
  #
  m =  glm( default ~ income + balance, data=Default, family="binomial", subset=training_samples )

  # Results from "predict" are in terms of log odds or the logit tranformation of the probabilities 
  predictions = predict( m, newdata=Default[validation_samples,] )

  default = factor( rep( "No", length(validation_samples) ), c("No","Yes") )
  default[ predictions > 0 ] = factor( "Yes", c("No","Yes") )

  validation_error_rate = mean( default != Default[validation_samples,]$default )
}

print( "Validation set error is:" )
print( validation_error_est() )

print( "Three more estimates of the validation set error would give:" )
print( validation_error_est() )
print( validation_error_est() )
print( validation_error_est() )


# Part (d):
#
validation_error_est = function(){
  #
  # Predictors are income, balance, AND student 
  #
  
  # (i):
  # 
  n = dim(Default)[1]
  training_samples = sample(1:n,floor(n/2))
  validation_samples = (1:n)[-training_samples] 

  # (ii):
  #
  m =  glm( default ~ income + balance + student, data=Default, family="binomial", subset=training_samples )

  # Results from "predict" are in terms of log odds or the logit tranformation of the probabilities 
  predictions = predict( m, newdata=Default[validation_samples,] )

  default = factor( rep( "No", length(validation_samples) ), c("No","Yes") )
  default[ predictions > 0 ] = factor( "Yes", c("No","Yes") )

  validation_error_rate = mean( default != Default[validation_samples,]$default )
}

print( "Using the predictor student our validation set error is:" )
print( validation_error_est() )



  






