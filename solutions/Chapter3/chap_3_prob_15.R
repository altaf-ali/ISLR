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
# EPage 140
# 
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }

library(MASS)

set.seed(1)

# Loop over each predictor and look for a statistically signficant simple linear regression: 
#
crim = Boston[,1]  
model_f_value = c()
model_p_value = c()
univariate_beta_value = c()
possible_predictors = colnames(Boston)
for( pi in 1:length(possible_predictors) ){
  if( possible_predictors[pi] == 'crim' ){ next }
  x = Boston[,pi] 
  m = lm( y ~ x, data=Boston )
  s = summary(m)
  model_f_value = c(model_f_value, s$fstatistic[1] )
  model_p_value = c(model_p_value, anova(m)$'Pr(>F)'[1] )
  univariate_beta_value = c(univariate_beta_value, coefficients(m)['x'])
  print( sprintf("%s %10.6f", possible_predictors[pi], coefficients(m)['x']) )
}

# Lets look at each models F-statisics:
# 
DF = data.frame( feature=colnames(Boston)[-1], f_values=model_f_value, p_values=model_p_value )
DF[order(model_f_value),] 
#
# Consider a model to significant if the p-value < 0.01.  Every model is signficant except one 
# this is chas = Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).

# Fit all of the predictors:
#
gm = lm( crim ~ ., data=Boston )
summary( gm )

multivariate_beta_values = coefficients(gm)
multivariate_beta_values = multivariate_beta_values[ possible_predictors[-1] ] # order the coefficients in the same order as the univariate coefficients 

# Plot these two coefficients:
#postscript("../../WriteUp/Graphics/Chapter3/prob_15_multi_as_fn_of_uni.eps", onefile=FALSE, horizontal=FALSE)
plot( univariate_beta_value, multivariate_beta_values )
#dev.off()

# Look for the possibility of including a non-linear term: 
plot( gm )

# See if we should fit a non-linear model of one variable (compare the ANOVA values):
# 
crim = Boston[,1]  
anova_f_value = c()
possible_predictors = colnames(Boston)
for( pi in 1:length(possible_predictors) ){
  if( possible_predictors[pi] == 'crim' ){ next }
  x = Boston[,pi]
  if( possible_predictors[pi] == 'chas' ){
    F = NA
  }else{
    m_1 = lm( y ~ x )
    m_3 = lm( y ~ poly(x,3) )
    F = anova(m_1,m_3)$F[2]
  }
  anova_f_value = c(anova_f_value, F)
}

DF = data.frame( feature=colnames(Boston)[-1], f_values=anova_f_value )
DF[order(anova_f_value),] 


# Lets look at how the non-linear model compares to the linear model:
# 
m_1 = lm( crim ~ medv, data=Boston )
m_3 = lm( crim ~ poly(medv,3), data=Boston )

#postscript("../../WriteUp/Graphics/Chapter3/prob_15_crim_vs_medv.eps", onefile=FALSE, horizontal=FALSE)
plot( crim ~ medv, data=Boston )
lines( Boston$medv, predict( m_1 ), col='red', type='p' )
lines( Boston$medv, predict( m_3 ), col='green', type='p' )
#dev.off()


