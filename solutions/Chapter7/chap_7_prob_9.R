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
# EPage 314
#
#-----

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("boot") ){ install.packages("boot") }
if( ! require("splines") ){ install.packages("splines") }

library(MASS)

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
#
m = lm( nox ~ poly(dis,3), data=Boston )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_9_part_a.eps", onefile=FALSE, horizontal=FALSE) }

plot( Boston$dis, Boston$nox, xlab='dis', ylab='nox', main='third degree polynomial fit' )

dis_range = range( Boston$dis )
dis_samples = seq( from=dis_range[1], to=dis_range[2], length.out=100 )
y_hat = predict( m, newdata=list( dis=dis_samples ) )

lines( dis_samples, y_hat, col='red' )
grid()

if( save_plots ){ dev.off() }


# Part (b-c):
#
d_max = 10

# The training RSS:
# 
training_rss = rep(NA,d_max)
for( d in 1:d_max ){
  m = lm( nox ~ poly(dis,d), data=Boston )
  training_rss[d] = sum( ( m$residuals )^2 )
}

# The RSS estimated using cross-validation:
# 
k = 10
folds = sample( 1:k, nrow(Boston), replace=TRUE ) 
cv.rss.test = matrix( NA, k, d_max )
cv.rss.train = matrix( NA, k, d_max )

for( d in 1:d_max ){
  for( fi in 1:k ){ # for each fold
    fit = lm( nox ~ poly(dis,d), data=Boston[folds!=fi,] )

    y_hat = predict( fit, newdata=Boston[folds!=fi,] )
    cv.rss.train[fi,d] = sum( ( Boston[folds!=fi,]$nox - y_hat )^2 ) 

    y_hat = predict( fit, newdata=Boston[folds==fi,] )
    cv.rss.test[fi,d] = sum( ( Boston[folds==fi,]$nox - y_hat )^2 )
  }
}

cv.rss.train.mean = apply(cv.rss.train,2,mean)
cv.rss.train.stderr = apply(cv.rss.train,2,sd)/sqrt(k)

cv.rss.test.mean = apply(cv.rss.test,2,mean)
cv.rss.test.stderr = apply(cv.rss.test,2,sd)/sqrt(k)

min_value = min( c(cv.rss.test.mean,cv.rss.train.mean) )
max_value = max( c(cv.rss.test.mean,cv.rss.train.mean) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_9_part_b_N_c.eps", onefile=FALSE, horizontal=FALSE) }

plot( 1:d_max, cv.rss.train.mean, xlab='polynomial degree', ylab='RSS', col='red', pch=19, type='b', ylim=c(min_value,max_value) )
lines( 1:d_max, cv.rss.test.mean, col='green', pch=19, type='b' )
grid()
legend( "topright", legend=c("train RSS","test RSS"), col=c("red","green"), lty=1, lwd=2 )

if( save_plots ){ dev.off() }


# Part (d-f):
#
m = lm( nox ~ bs(dis,df=4), data=Boston )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_9_part_d_bs_fit.eps", onefile=FALSE, horizontal=FALSE) }

plot( Boston$dis, Boston$nox, xlab='dis', ylab='nox', main='bs with df=4 fit' )

dis_range = range( Boston$dis )
dis_samples = seq( from=dis_range[1], to=dis_range[2], length.out=100 )
y_hat = predict( m, newdata=list( dis=dis_samples ) )

lines( dis_samples, y_hat, col='red' )
grid()

if( save_plots ){ dev.off() }



dof_choices = c(3,4,5,10,15,20)
n_dof_choices = length(dof_choices)

# The RSS estimated using cross-validation:
# 
k = 5
folds = sample( 1:k, nrow(Boston), replace=TRUE ) 
cv.rss.test = matrix( NA, k, n_dof_choices )
cv.rss.train = matrix( NA, k, n_dof_choices )

for( di in 1:n_dof_choices ){
  for( fi in 1:k ){ # for each fold
    fit = lm( nox ~ bs(dis,df=dof_choices[di]), data=Boston[folds!=fi,] )

    y_hat = predict( fit, newdata=Boston[folds!=fi,] )
    cv.rss.train[fi,di] = sum( ( Boston[folds!=fi,]$nox - y_hat )^2 ) 

    y_hat = predict( fit, newdata=Boston[folds==fi,] )
    cv.rss.test[fi,di] = sum( ( Boston[folds==fi,]$nox - y_hat )^2 )
  }
}

cv.rss.train.mean = apply(cv.rss.train,2,mean)
cv.rss.train.stderr = apply(cv.rss.train,2,sd)/sqrt(k)

cv.rss.test.mean = apply(cv.rss.test,2,mean)
cv.rss.test.stderr = apply(cv.rss.test,2,sd)/sqrt(k)

min_value = min( c(cv.rss.test.mean,cv.rss.train.mean) )
max_value = max( c(cv.rss.test.mean,cv.rss.train.mean) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_9_part_d_T_f.eps", onefile=FALSE, horizontal=FALSE) }

plot( dof_choices, cv.rss.train.mean, xlab='spline d.o.f.', ylab='RSS', col='red', pch=19, type='b', ylim=c(min_value,max_value) )
lines( dof_choices, cv.rss.test.mean, col='green', pch=19, type='b' )
grid()
legend( "topright", legend=c("train RSS","test RSS"), col=c("red","green"), lty=1, lwd=2 )

if( save_plots ){ dev.off() }

