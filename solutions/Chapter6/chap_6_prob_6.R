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
#-----

y_1 = 10
lambda = 4

#postscript("../../WriteUp/Graphics/Chapter6/prob_6_Eq_6_12.eps", onefile=FALSE, horizontal=FALSE)

betas = seq( from=-5, to=+7, length.out=200 )
plot( betas, ( y_1 - betas )^2 + lambda * betas^2 )
grid()

#dev.off()

y_1 / ( 1 + lambda )



#postscript("../../WriteUp/Graphics/Chapter6/prob_6_Eq_6_13.eps", onefile=FALSE, horizontal=FALSE)

betas = seq( from=-1, to=+10, length.out=200 )
plot( betas, ( y_1 - betas )^2 + lambda * abs(betas) )
grid()

#dev.off()



