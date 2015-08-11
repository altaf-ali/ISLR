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
# EPage 429
#
#-----

save_plots = F

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

DM = matrix( data=c(0.0,0.3,0.4,0.7,0.3,0.0,0.5,0.8,0.4,0.5,0.0,0.45,0.7,0.8,0.45,0.0), nrow=4, ncol=4, byrow=TRUE )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_2_complete_linkage.eps", onefile=FALSE, horizontal=FALSE) }
plot( hclust( as.dist( DM ), method="complete" ), main="complete linkage" )
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_2_single_linkage.eps", onefile=FALSE, horizontal=FALSE) }
plot( hclust( as.dist( DM ), method="single" ), main="single linkage" )
if( save_plots ){ dev.off() }

