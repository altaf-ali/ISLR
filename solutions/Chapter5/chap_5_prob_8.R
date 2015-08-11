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

# Part (a):
#
set.seed(1)

x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

#postscript("../../WriteUp/Graphics/Chapter5/prob_8_scatter_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( x, y )
#dev.off()

DF = data.frame( y=y, x=x )

# Do cross-validation on each model:
# 
m_i = glm( y ~ x, data=DF )
cv.err = cv.glm( DF, m_i )
print( sprintf( "Model (i): cv output= %10.6f", cv.err$delta[1] ) )

m_ii = glm( y ~ x + I(x^2), data=DF )
cv.err = cv.glm( DF, m_ii )
print( sprintf( "Model (ii): cv output= %10.6f", cv.err$delta[1] ) )

m_iii = glm( y ~ x + I(x^2) + I(x^3), data=DF )
cv.err = cv.glm( DF, m_iii )
print( sprintf( "Model (iii): cv output= %10.6f", cv.err$delta[1] ) )

m_iv = glm( y ~ x + I(x^2) + I(x^3) + I(x^4), data=DF )
cv.err = cv.glm( DF, m_iv )
print( sprintf( "Model (iv): cv output= %10.6f", cv.err$delta[1] ) )








