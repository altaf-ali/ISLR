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
# EPage 140
# 
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }

set.seed(1)

x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# Look at the correlation between x_1 and x_2:
# 
cor( x1, x2 )

#postscript("../../WriteUp/Graphics/Chapter3/prob_14_scatter_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( x1, x2 ) 
#dev.off()

summary( lm( y ~ x1 + x2 ) )

summary( lm( y ~ x1 ) )

summary( lm( y ~ x2 ) )

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y,6)

# Consider what each model thinks about the mismeasured point: 
plot( lm( y ~ x1 + x2 ) ) # 101 is a high-leverage point in this model 
plot( lm( y ~ x1 ) ) # 101 is a outlier and a high-leverage point in this model 
plot( lm( y ~ x2 ) ) # 101 is a high-leverage point in this model 

