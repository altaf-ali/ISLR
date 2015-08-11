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

# Estimate the base model (to get standard errors of the coefficients):
# 
m0 = glm( default ~ income + balance, data=Default, family="binomial" )
summary(m0)


boot.fn = function(data,index){
  m = glm( default ~ income + balance, data=data[index,], family="binomial" )
  return( coefficients(m) )
}
boot.fn(Default,1:10000) # test our boot function

boot( Default, boot.fn, 1000 )





