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
# EPage 187
#
#-----

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("class") ){ install.packages("class") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

Auto = read.csv("../../Data/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)
Auto$name = NULL 

# Part (a):
#
mpg01 = rep( 0, dim(Auto)[1] ) # 0 => less than the median of mpg 
mpg01[ Auto$mpg > median(Auto$mpg) ] = 1 # 1 => greater than the median of mpg 

Auto$mpg01 = mpg01
Auto$mpg = NULL 

# Part (b):
#
print( cor( Auto ) )
pairs( Auto )

Auto$mpg01 = as.factor( mpg01 )

# Part (c):
#
n = dim(Auto)[1]
inds.train = sample(1:n,3*n/4)
Auto.train = Auto[inds.train,]
inds.test = (1:n)[-inds.train] 
Auto.test = Auto[inds.test,]

# Part (d) Use LDA:
#
lda.fit = lda( mpg01 ~ cylinders + displacement + weight, data=Auto.train )

lda.predict = predict( lda.fit, newdata=Auto.test )
CM = table( predicted=lda.predict$class, truth=Auto.test$mpg01 )
print( CM )
print( sprintf( "LDA: overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

# Part (e): Use QDA:
#
qda.fit = qda( mpg01 ~ cylinders + displacement + weight, data=Auto.train )

qda.predict = predict( qda.fit, newdata=Auto.test )
CM = table( predicted=qda.predict$class, truth=Auto.test$mpg01 )
print( CM )
print( sprintf( "QDA: overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

# Part (f): Use Logistic regression:
#
lr.fit = glm( mpg01 ~ cylinders + displacement + weight, data=Auto.train, family=binomial )

p_hat = predict( lr.fit, newdata=Auto.test, type="response" )
y_hat = rep( 0, length(p_hat) )
y_hat[ p_hat > 0.5 ] = 1
CM = table( predicted=as.factor( y_hat ), truth=Auto.test$mpg01 )
print( CM )
print( sprintf( "LR (all features): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )


