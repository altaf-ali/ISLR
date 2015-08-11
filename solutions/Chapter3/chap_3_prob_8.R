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

Auto = read.csv("../../Data/Auto.csv",header=T,na.strings="?")
Auto = na.omit(Auto)

qualitative_columns = c(2,8,9)

fit1 = lm( mpg ~ horsepower, data=Auto ) 

#postscript("../../WriteUp/Graphics/Chapter3/prob_8_mpg_as_fn_of_horsepower_plot.eps", onefile=FALSE, horizontal=FALSE)
plot( mpg ~ horsepower, Auto )
abline(fit1,col='red')
#dev.off()

plot( fit1 )



