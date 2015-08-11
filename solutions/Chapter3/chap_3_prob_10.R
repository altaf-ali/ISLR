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


fit_1 = lm( Sales ~ Price + Urban + US, data=Carseats )
summary( fit_1 )

fit_2 = update( fit, . ~ . - Urban )

confint( fit_2, level=0.95 )
