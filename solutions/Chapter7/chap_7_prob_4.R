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

X = seq( from=-2, to=+8, length.out=500 )

# Compute some auxilary indicator functions: 
I_1 = ( X >= 0 ) & ( X <= 2 )
I_2 = ( X >= 1 ) & ( X <= 2 )
I_3 = ( X >= 3 ) & ( X <= 4 )
I_4 = ( X >= 4 ) & ( X <= 5 ) 

Y = 1 + ( I_1 - ( X - 1 ) * I_2 ) + 3 * ( ( X - 3 ) * I_3 + I_4 ) 

#postscript("../../WriteUp/Graphics/Chapter7/prob_4_plot.eps", onefile=FALSE, horizontal=FALSE)

plot( X, Y, type='l' )
grid()

#dev.off()






