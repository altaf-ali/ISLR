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
# EPage 350
#
#-----

save_plots = FALSE

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("gbm") ){ install.packages("gbm") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("randomForest") ){ install.packages("randomForest") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

Caravan = na.omit(Caravan)

n = nrow(Caravan)
p = ncol(Caravan) - 1 # one column is the response we are trying to model i.e. "Purchase" 

train = 1:1000
test = 1001:n

# Transform the response "Purchase" to be in [0,1] as required by gbm:
#
PurchaseBinary = rep(0,n)
PurchaseBinary[ Caravan$Purchase == 'Yes' ] = 1 
Caravan$Purchase = PurchaseBinary

# Some variables seem to be very noninformative (have zero variance as reported by gbm):
#
Caravan$PVRAAUT = NULL
Caravan$AVRAAUT = NULL

# Train a gbm:
# 
lm = 0.01
boost.caravan = gbm( Purchase ~ ., data=Caravan[train,], distribution="bernoulli", n.trees=1000, interaction.depth=2, shrinkage=lm )
summary( boost.caravan )

# Predict the testing error:
# 
y_hat = predict( boost.caravan, newdata=Caravan[test,], n.trees=1000 )
p_hat = exp( y_hat ) / ( 1 + exp( y_hat ) ) # convert the logodd output into probabilities 

will_buy = rep( 0, length(test) )
will_buy[ p_hat > 0.2 ] = 1

# Create a confusion matrix:
# 
table( will_buy, Caravan[test,]$Purchase )

# Train a logistic regression:
#
lr_model = glm( Purchase ~ ., data=Caravan[train,], family="binomial" )
y_hat = predict( lr_model, newdata=Caravan[test,] )
p_hat = exp( y_hat ) / ( 1 + exp( y_hat ) ) # convert the logodd output into probabilities 

will_buy = rep( 0, length(test) )
will_buy[ p_hat > 0.2 ] = 1

# Create a confusion matrix:
# 
table( will_buy, Caravan[test,]$Purchase )

# Try bagging (not implemented/tested): 
#
if( FALSE ){ 
  bag.hitters = randomForest( Purchase ~ ., data=Caravan, mtry=p, ntree=1000, importance=TRUE, subset=train )
  y_hat = predict( bag.hitters, newdata=Caravan[test,] )
  mse.bag = mean( ( Caravan[test,]$Purchase - y_hat )^2 )
  print( 'randomForest test MSE:' )
  print( mse.bag )
}

