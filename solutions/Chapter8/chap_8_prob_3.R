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
# EPage 347
#
#-----

save_plots = FALSE

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

p1 = seq( 0+1e-6, 1-1e-6, length.out=100 )
p2 = 1 - p1

# The missclassification error-rate:
#
E = 1 - apply( rbind( p1, p2 ), 2, max )

# The Gini index:
#
G = p1 * (1-p1) + p2 * (1-p2) 

# The cross-entropy:
#
D = - ( p1 * log( p1 ) + p2 * log( p2 ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_3_error_metrics.eps", onefile=FALSE, horizontal=FALSE) }
plot( p1, E, type='l', col='black', xlab='p_1', ylab='value of error metric', ylim=c(min(c(E,G,D)),max(E,G,D)) )
lines( p1, G, col='blue' )
lines( p1, D, col='green' )
legend( 0.2, 0.1, c('Classification error','Gini index','Cross entropy'), col=c('black','blue','green'), lty=c(1,1) )
grid()
if( save_plots ){ dev.off() }

