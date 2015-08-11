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
# EPage 347 problem 
# EPage 342 lab on Carseats data
#
#-----

save_plots = FALSE

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("tree") ){ install.packages("tree") }
if( ! require("randomForest") ){ install.packages("randomForest") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

n = nrow(Carseats)
p = ncol(Carseats)-1 # remove the column we seek to predict i.e. Sales

# Part (a):
#
train = sample(1:n, n/2)
test = (1:n)[-train]

# Part (b):
#
rtree.carseats = tree( Sales ~ ., data=Carseats[train,] )
summary(rtree.carseats)

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_8_original_tree.eps", onefile=FALSE, horizontal=FALSE) }

plot(rtree.carseats)
text(rtree.carseats,pretty=0)

if( save_plots ){ dev.off() }

print( rtree.carseats )

y_hat = predict( rtree.carseats, newdata=Carseats[test,] )
test.MSE = mean( ( y_hat - Carseats[test,]$Sales )^2 )
print( test.MSE )

# Part (c):
#
# Use cross-validation to determine the optimal of tree complexity:
# 
cv.carseats = cv.tree( rtree.carseats )
names( cv.carseats )

print( cv.carseats )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_8_CV_results.eps", onefile=FALSE, horizontal=FALSE) }
plot( cv.carseats$size, cv.carseats$dev, type="b") # plot the tree size
if( save_plots ){ dev.off() }

# Pick the size of the tree you want to prune to:
# 
# It looks like k=6 is the smallest tree with an error close to the minimum.
# 

prune.carseats = prune.tree( rtree.carseats, best=6 ) 

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_8_pruned_tree.eps", onefile=FALSE, horizontal=FALSE) }

plot(prune.carseats)
text(prune.carseats,pretty=0)

if( save_plots ){ dev.off() }

# Predict the MSE using this tree:
# 
y_hat = predict( prune.carseats, newdata=Carseats[test,] )
prune.MSE = mean( ( y_hat - Carseats[test,]$Sales )^2 )
print( prune.MSE )

# Part (d):
#
# Use bagging
# 
carseats.bag = randomForest( Sales ~ ., data=Carseats, mtry=p, ntree=500, importance=TRUE, subset=train )
y_hat = predict( carseats.bag, newdata=Carseats[test,] )
mse.bag = mean( ( Carseats[test,]$Sales - y_hat )^2 )
print( mse.bag )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_8_bagged_trees.eps", onefile=FALSE, horizontal=FALSE) }
plot(carseats.bag)
if( save_plots ){ dev.off() }

ibag = importance( carseats.bag )
print( ibag[ order( ibag[,1] ), ] )


# Part (e):
# 
# Use random forests
#
carseats.rf = randomForest( Sales ~ ., data=Carseats, ntree=500, mtry=p/3, importance=TRUE, subset=train )
y_hat = predict( carseats.rf, newdata=Carseats[test,] )
mse.rf = mean( ( Carseats[test,]$Sales - y_hat )^2 )
print( mse.rf )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_8_rf_trees.eps", onefile=FALSE, horizontal=FALSE) }
plot(carseats.rf)
if( save_plots ){ dev.off() }

irf = importance( carseats.rf )
print( irf[ order( irf[,1] ), ] )


