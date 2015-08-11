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

set.seed(1)
n = 100
x = rnorm(n)
y = 2 * x + rnorm(n)

# Part (a): 
fit = lm( y ~ x + 0 )
summary(fit)

# Part (b): 
fit = lm( x ~ y + 0 )
summary(fit)

# Part (f): verify that these two regressions give the same t-statistic
fit_x_to_y = lm( x ~ y ) 
summary(fit_x_to_y)
fit_y_to_x = lm( y ~ x ) 
summary(fit_y_to_x)

