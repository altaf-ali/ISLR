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
# EPage 137
# 
#-----

Auto = read.csv("../../Data/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)
Auto$name = NULL 

qualitative_columns = c(2,8,9)

pairs(Auto) # almost too complicated to work with

fit = lm( mpg~., data=Auto )
summary(fit)

plot(fit)

# Use update to add some interaction terms:
#
summary( update( fit, . ~ . + horsepower:weight ) )

# Lets see if this is indeed a better model:
# 
anova( fit, update( fit, . ~ . + horsepower:weight ) )

# Use update to add some nonlinear terms:
# Note that other terms i.e. displacement, weight, and accelation should also have a significant non-linear term: 
#
summary( update( fit, . ~ . + I(horsepower^2) ) )





