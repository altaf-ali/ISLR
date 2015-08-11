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

summary(Auto)

qualitative_columns = c(2,8,9)

sapply( Auto[,-qualitative_columns], range )

sapply( Auto[,-qualitative_columns], mean )
sapply( Auto[,-qualitative_columns], sd )

# Part (d):
#
sapply( Auto[-seq(10,85),-qualitative_columns], mean )

# Part (e):
#
#postscript("../../WriteUp/Graphics/Chapter2/prob_9_pairs_plot.eps", onefile=FALSE, horizontal=FALSE)
pairs( Auto[,-qualitative_columns] )
#dev.off()

#postscript("../../WriteUp/Graphics/Chapter2/prob_9_mpg_vs_year.eps", onefile=FALSE, horizontal=FALSE)
plot( Auto$year, Auto$mpg ) 
#dev.off()

# Lets plot some mpg vs. some of our qualitative features:
#
#postscript("../../WriteUp/Graphics/Chapter2/prob_9_mpg_vs_cylinders.eps", onefile=FALSE, horizontal=FALSE)
plot( as.factor(Auto$cylinders), Auto$mpg )
#dev.off()

#postscript("../../WriteUp/Graphics/Chapter2/prob_9_mpg_vs_origin.eps", onefile=FALSE, horizontal=FALSE)
plot( as.factor(Auto$origin), Auto$mpg ) 
#dev.off()



