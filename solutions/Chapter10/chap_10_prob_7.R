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
# EPage 431
#
#-----

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Scale each observation (not the features):
# 
USA_scaled = t(scale(t(USArrests)))

# The correlation of each sample with the other samples:
# 
Rij = cor(t(USA_scaled)) # -1 <= Rij <= +1 
OneMinusRij = 1 - Rij # 0 <= 1-Rij <= +2 
X = OneMinusRij[lower.tri(OneMinusRij)]

D = as.matrix( dist( USA_scaled )^2 )
Y = D[lower.tri(D)]

plot( X, Y )

summary( X/Y )









