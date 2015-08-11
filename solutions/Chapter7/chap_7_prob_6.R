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

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("boot") ){ install.packages("boot") }

set.seed(0)

# Plot the data to see what it looks like:
# 
with( Wage, plot( age, wage ) )


# Part (a):
#

# Perform polynomial regression for various polynomial degrees:
# 
cv.error = rep(0,10)
for( i in 1:10 ){ # fit polynomial models of various degrees (based on EPage 208 in the book)
  glm.fit = glm( wage ~ poly(age,i), data=Wage )
  cv.error[i] = cv.glm( Wage, glm.fit, K=10 )$delta[1]
}

#postscript("../../WriteUp/Graphics/Chapter7/prob_6_part_a_CV_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( 1:10, cv.error, pch=19, type='b', xlab='degree of polynomial', ylab='CV estimate of the prediction error' )
grid()
#dev.off()

# Using the minimal value for the CV error gives the value 10 which seems like too much polynomial i.e. too wiggly
# From the plot 4 is the point where the curve stops decreasing and starts increasing so we will consider polynomials
# of this degree.
# 
me = which.min( cv.error )
me = 4

m = glm( wage ~ poly(age,me), data=Wage ) 

#postscript("../../WriteUp/Graphics/Chapter7/prob_6_part_a_data_N_model_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( Wage$age, Wage$wage )

aRng = range(Wage$age)

a_predict = seq( from=aRng[1], to=aRng[2], length.out=100 )
w_predict = predict( m, newdata=list( age=a_predict ) )
lines( a_predict, w_predict, col='red' )
#dev.off()


# Lets consider the ANOVA approach (i.e. a sequence of nested linear models):
#
m0 = lm( wage ~ 1, data=Wage )
m1 = lm( wage ~ poly(age,1), data=Wage )
m2 = lm( wage ~ poly(age,2), data=Wage )
m3 = lm( wage ~ poly(age,3), data=Wage )
m4 = lm( wage ~ poly(age,4), data=Wage )
m5 = lm( wage ~ poly(age,5), data=Wage )
anova(m0,m1,m2,m3,m4,m5)


# Part (b):
#

# Lets due the same thing with the cut function for fitting a piecewise constant model:
#
# For some reason the command cv.glm does not work when we use the "cut" command.  I think it was
# how I formed the cut factors the first time i.e. not taking bins when some of the testing data
# was outside of the training data.  
#
# To debug this and understand what is going on we will do cross-validation by hand.
# See EPage 265 in the book on some more information on how to do cross-validation in R.
#
number_of_bins = c( 2, 3, 4, 5, 10 )
nc = length(number_of_bins)

k = 10
folds = sample( 1:k, nrow(Wage), replace=TRUE ) 
cv.errors = matrix( NA, k, nc )

# Prepare for the type of factors you might obtain (extend the age range a bit): 
# 
age_range = range( Wage$age )
age_range[1] = age_range[1]-1
age_range[2] = age_range[2]+1

for( ci in 1:nc ){ # for each number of cuts to test
  nob = number_of_bins[ci] # n(umber) o(f) c(uts) 2, 3, 4 ...

  for( fi in 1:k ){ # for each fold

    # In this ugly command we:
    # 
    # break the "age" variable in the subset of data Wage[folds!=fi,] into "nob" bins that
    # span between the smallest and largest values of age observed over the entire dataset.
    #
    # This allows us to be able to use the function "predict" on age values not seen in
    # the training subset.
    #
    # If we try to "cut" the age variable into bins that are too small they may not contain any ages
    # in them.  I'm not sure that lm/glm would be doing something reasonable in that case.  Thus I only
    # do cross-validation on a smallish number of bins.
    # 
    fit = glm( wage ~ cut( age, breaks=seq( from=age_range[1], to=age_range[2], length.out=(nob+1) ) ), data=Wage[folds!=fi,] )
    y_hat = predict( fit, newdata=Wage[folds==fi,] )
    cv.errors[fi,ci] = mean( ( Wage[folds==fi,]$wage - y_hat )^2 ) 
  }
  
}

cv.errors.mean = apply(cv.errors,2,mean)
cv.errors.stderr = apply(cv.errors,2,sd)/sqrt(k)

min.cv.index = which.min( cv.errors.mean )
one_se_up_value = ( cv.errors.mean+cv.errors.stderr )[min.cv.index] 

#postscript("../../WriteUp/Graphics/Chapter7/prob_6_part_b_CV_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( number_of_bins, cv.errors.mean, ylim=c(min_lim,max_lim), pch=19, type='b', xlab='number of cut bins', ylab='CV estimate of the prediction error' )
lines( number_of_bins, cv.errors.mean-cv.errors.stderr, lty='dashed' )
lines( number_of_bins, cv.errors.mean+cv.errors.stderr, lty='dashed' )
abline( h=one_se_up_value, col='red' )
grid()
#dev.off()

# Fit the optimal model using all data:
# 
nob = 3
fit = glm( wage ~ cut( age, breaks=seq( from=age_range[1], to=age_range[2], length.out=(nob+1) ) ), data=Wage )

#postscript("../../WriteUp/Graphics/Chapter7/prob_6_part_b_data_N_model_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( Wage$age, Wage$wage )

aRng = range(Wage$age)

a_predict = seq( from=aRng[1], to=aRng[2], length.out=100 )
w_predict = predict( fit, newdata=list( age=a_predict ) )
lines( a_predict, w_predict, col='red', lw=4 )
#dev.off()

