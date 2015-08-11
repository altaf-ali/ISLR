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

save_plots = FALSE

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

pr.out = prcomp(USArrests, scale=TRUE)

# Using the output from prcomp:
# 
pr.var = pr.out$sdev^2
pve_1 = pr.var / sum(pr.var)

# Apply Equation 10.8 directly:
#
USArrests_scaled = scale( USArrests )
denom = sum( apply( USArrests_scaled^2, 2, sum ) )

Phi = pr.out$rotation
USArrests_projected = USArrests_scaled %*% Phi # this is the same as pr.out$x

numer = apply( pr.out$x^2, 2, sum )
pve_2 = numer / denom

print(pve_1)
print(pve_2)
print(pve_1 - pve_2)
