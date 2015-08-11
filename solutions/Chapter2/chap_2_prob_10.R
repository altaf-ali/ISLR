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
# EPage 71
# 
#-----

library(MASS)

all_correlations = cor( Boston )
print( all_correlations[,14] )

cols = c(14,13,6,11,3,10)

#postscript("../../WriteUp/Graphics/Chapter2/prob_10_pairs_plot.eps", onefile=FALSE, horizontal=FALSE)
pairs( Boston[,cols] )
#dev.off()

# Look how the variable most correlated to crim (turns out to be rad)
plot( Boston[,1], Boston[,9] )


