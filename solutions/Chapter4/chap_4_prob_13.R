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

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("class") ){ install.packages("class") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

n = dim(Boston)[1]

# Introduce a variable whether or not the crime rate is above=1 / below=0 the median 
Boston$crim01 = rep( 0, n )
Boston$crim01[ Boston$crim >= median( Boston$crim ) ] = 1
Boston$crim = NULL 

# Look to see what features are most strongly correlated with crim01:
#
Boston.cor = cor( Boston )
print( sort( Boston.cor[,'crim01'] ) )

# Split the data set into testing and training parts:
inds.train = sample(1:n,3*n/4)
inds.test = (1:n)[-inds.train]
Boston.train = Boston[inds.train,]
Boston.test = Boston[inds.test,]

# Fit several models to the training data 
#
lr_model = glm( crim01 ~ nox + rad + dis, data=Boston.train, family=binomial )

p_hat = predict( lr_model, newdata=Boston.test, type="response" )
y_hat = rep( 0, length(p_hat) )
y_hat[ p_hat > 0.5 ] = 1
CM = table( predicted=y_hat, truth=Boston.test$crim01 )
print( CM )
print( sprintf( "LR: overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

lda.fit = lda( crim01 ~ nox + rad + dis, data=Boston.train )

# Use LDA
#
lda.fit = lda( crim01 ~ nox + rad + dis, data=Boston.train )

lda.predict = predict( lda.fit, newdata=Boston.test )
CM = table( predicted=lda.predict$class, truth=Boston.test$crim01 )
print( CM )
print( sprintf( "LDA: overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )


#f: Use QDA
#
qda.fit = qda( crim01 ~ nox + rad + dis, data=Boston.train )

qda.predict = predict( qda.fit, newdata=Boston.test ) 
CM = table( predicted=qda.predict$class, truth=Boston.test$crim01 )
print( CM )
print( sprintf( "QDA: overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

#g: KNN 
#
X.train = Boston.train; X.train$"crim01" = NULL 
Y.train = Boston.train$"crim01"

X.test = Boston.test; X.test$"crim01" = NULL 

y_hat_k_1 = knn( X.train, X.test, Y.train, k=1 )

CM = table( predicted=y_hat_k_1, truth=Boston.test$crim01 ) 
print( CM )
print( sprintf( "KNN (k=1): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

y_hat_k_3 = knn( X.train, X.test, Y.train, k=3 )
CM = table( predicted=y_hat_k_3, truth=Boston.test$crim01 ) 
print( CM )
print( sprintf( "KNN (k=3): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )








