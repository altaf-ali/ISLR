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
# EPage 139
# 
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }

set.seed(1)

# Different coefficient estimates in regressions y ~ x and x ~ y:
# 
x = rnorm(100)
y = 2 * x + rnorm(100)

coef( lm( y ~ x ) )
coef( lm( x ~ y ) )

# The same coefficient estimates in regressions y ~ x and x ~ y:
#
x = rnorm(100)
y = x 

coef( lm( y ~ x ) )
coef( lm( x ~ y ) )

