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
# EPage 386
#
#-----

save_plots = F

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("e1071") ){ install.packages("e1071") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

Auto = read.csv("../../Data/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)
Auto$name = NULL 

# Part (a):
#
if( TRUE ){
  AbvMedian = rep( 0, dim(Auto)[1] ) # 0 => less than the median of mpg 
  AbvMedian[ Auto$mpg > median(Auto$mpg) ] = 1 # 1 => greater than the median of mpg 
}else{
  AbvMedian = rep( "LT", dim(Auto)[1] )
  AbvMedian[ Auto$mpg > median(Auto$mpg) ] = "GT"
}
AbvMedian = as.factor(AbvMedian)
Auto$AbvMedian = AbvMedian
Auto$mpg = NULL 

# Part (b):
#
tune.out = tune( svm, AbvMedian ~ ., data=Auto, kernel="linear", ranges=list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

# Some plots to explore with:
# 
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_7_linear_SVM_weight_vs_horsepower.eps", onefile=FALSE, horizontal=FALSE) }
plot( tune.out$best.model, Auto, weight ~ horsepower )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_7_linear_SVM_cylinders_vs_year.eps", onefile=FALSE, horizontal=FALSE) }
plot( tune.out$best.model, Auto, cylinders ~ year ) 
if( save_plots ){ dev.off() }

plot( tune.out$best.model, Auto, cylinders ~ horsepower )

# Part (c) radial kernel:
#
tune.out = tune( svm, AbvMedian ~ ., data=Auto, kernel="radial", ranges = 
	         list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                       gamma=c(0.5,1,2,3,4) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

plot( tune.out$best.model, Auto, weight ~ horsepower )
plot( tune.out$best.model, Auto, cylinders ~ year ) 

# Part (c) polynomial kernel:
#
tune.out = tune( svm, AbvMedian ~ ., data=Auto, kernel="polynomial", ranges = 
	         list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                       degree=c(1,2,3,4,5) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

