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
# EPage 69
# 
#-----

college = read.csv('../../Data/College.csv')

rownames(college) = college[,1]
college = college[,-1] 

fix(college)

summary(college)

pairs( college[,1:10] )

# plot side by side boxplots of Outstate versus Private (Private universities have more out of state students): 
plot( college$Private, college$Outstate )

Elite = rep("No",nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame( college, Elite )

summary( Elite )

# plot side by side boxplots of Outstate versus Elite (Elite universities have more out of state students): 
plot( college$Elite, college$Outstate )

# Part vi (Some interesting observations):
#
college[which.max(college$Top10perc),] # what is the university with the most students in the top 10% of class

acceptance_rate = college$Accept / college$Apps

college[ which.min( acceptance_rate ), ] # what university has the smallest acceptance rate

college[ which.max( acceptance_rate ), ] # what university has the most liberal acceptance rate






