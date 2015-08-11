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

x = rnorm(100)
eps = rnorm( 100, mean=0, sd=sqrt(0.25) )
y_pure = -1 + 0.5 * x 
y = y_pure + eps

#postscript("../../WriteUp/Graphics/Chapter3/prob_13_linear_fit.eps", onefile=FALSE, horizontal=FALSE)

plot( x, y )

fit = lm( y ~ x )
summary( fit ) 

abline( fit )
abline( a=-1, b=1/2, col='green' ) 
legend( -3, 1, c("estimated","truth"), col=c("black","green"), lty=c(1,1) )

#dev.off()


# Try to fit a quadradic model:
#
qfit = lm( y ~ x + I(x^2) )
summary( qfit )


# Fit different amounts of noise:
# 
confint( fit, level=0.95 )

eps_less = rnorm( 100, mean=0, sd=sqrt(0.1) )
y = y_pure + eps_less 
confint( lm( y ~ x ), level=0.95 )

eps_more = rnorm( 100, mean=0, sd=sqrt(0.5) )
y = y_pure + eps_more
confint( lm( y ~ x ), level=0.95 )
