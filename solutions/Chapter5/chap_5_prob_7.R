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

set.seed(0)

# Part (a):
m_0 = glm( Direction ~ Lag1 + Lag2, data=Weekly, family="binomial" )

# Part (b):
m_loocv = glm( Direction ~ Lag1 + Lag2, data=Weekly[-1,], family="binomial" )

print( sprintf( "Prediction on first sample is %d (1=>Up; 0=>Down)", predict( m_loocv, newdata=Weekly[1,] ) > 0 ) )
print( sprintf( "First samples true direction is %s", Weekly[1,]$Direction ) )

n = dim(Weekly)[1]
number_of_errors = 0
for( ii in 1:n ){
  m_loocv = glm( Direction ~ Lag1 + Lag2, data=Weekly[-ii,], family="binomial" )

  error_1 = ( predict( m_loocv, newdata=Weekly[ii,] ) > 0 ) & ( Weekly[ii,]$Direction == "Down" )
  error_2 = ( predict( m_loocv, newdata=Weekly[ii,] ) < 0 ) & ( Weekly[ii,]$Direction == "Up" )
  if( error_1 | error_2 ){
    number_of_errors = number_of_errors + 1 
  }
}
print( sprintf( "LOOCV test error rate= %10.6f", number_of_errors/n ) )






