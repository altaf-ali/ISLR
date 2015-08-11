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
# EPage 187
#
#-----

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
#
Power = function(){
  print( 2^3 )
}

# Part (b):
#
Power2 = function(x,a){
  print( x^a ) 
}

