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

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("tree") ){ install.packages("tree") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

n = nrow(OJ)
p = ncol(OJ)-1 # remove the response Purchase

# Part (a):
#
train = sample(1:n, 800)
test = (1:n)[-train]

# Part (b):
#
tree.OJ = tree( Purchase ~ ., data=OJ[train,] )
summary(tree.OJ)

print( tree.OJ )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_9_original_tree.eps", onefile=FALSE, horizontal=FALSE) }
plot(tree.OJ)
text(tree.OJ,pretty=0)
if( save_plots ){ dev.off() }

y_hat = predict( tree.OJ, newdata=OJ[test,], type="class" ) # gives classification labels
CT = table( y_hat, OJ[test,]$Purchase )
print( CT )
print( 'original tree: classificaion error rate on the test dataset:')
print( ( CT[1,2] + CT[2,1] ) / sum(CT) )

# Part (c):
#
# Use cross-validation to determine the optimal of tree complexity:
# 
cv.OJ = cv.tree( tree.OJ )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_9_CV_results.eps", onefile=FALSE, horizontal=FALSE) }
plot( cv.OJ$size, cv.OJ$dev, type="b")
if( save_plots ){ dev.off() }

# Pick the size of the tree you want to prune to:

prune.OJ = prune.tree( tree.OJ, best=5 )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_9_pruned_tree.eps", onefile=FALSE, horizontal=FALSE) }
plot(prune.OJ)
text(prune.OJ,pretty=0)
if( save_plots ){ dev.off() }

# Compute training error rates:
# 
y_hat = predict( prune.OJ, newdata=OJ[train,], type="class" )
CT = table( y_hat, OJ[train,]$Purchase )
print( 'pruned tree: classificaion error rate on the training dataset:')
print( ( CT[1,2] + CT[2,1] ) / sum(CT) )

# Compute testing error rates:
#
y_hat = predict( prune.OJ, newdata=OJ[test,], type="class" )
CT = table( y_hat, OJ[test,]$Purchase )
print( 'pruned tree: classificaion error rate on the test dataset:')
print( ( CT[1,2] + CT[2,1] ) / sum(CT) )
