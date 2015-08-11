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

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

X = seq( from=-4, to=+4, length.out=500 )

Y = 1 + X - 2 * ( X-1 )^2 * ( X >= 1 )

#postscript("../../WriteUp/Graphics/Chapter7/prob_3_plot.eps", onefile=FALSE, horizontal=FALSE)

plot( X, Y, type='l' )
abline(v=1,col='red')
grid()

#dev.off()






