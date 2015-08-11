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
# EPage 429
#
#-----

save_plots = F

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
#
DF = data.frame( x1=c(1,1,0,5,6,4), x2=c(4,3,4,1,2,0) )

n = dim(DF)[1]
K = 2

# Part (b): Assign each point to a cluster
#
labels = sample( 1:K, n, replace=TRUE )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_3_initial_plot.eps", onefile=FALSE, horizontal=FALSE) }
plot( DF$x1, DF$x2, cex=2.0, pch=19, col=(labels+1), xlab="gene index", ylab="unpaired t-value" )
grid()
if( save_plots ){ dev.off() }

while( TRUE ){

    # Part (c): Compute the centroids of each cluster
    #
    cents = matrix( nrow=K, ncol=2 )
    for( l in 1:K ){
      samps = labels==l
      cents[l,] = apply( DF[samps,], 2, mean ) 
    }

    # Part (d): Assign each sample to the centroid it is closest too:
    #
    new_labels = rep( NA, n )
    for( si in 1:n ){
      smallest_norm = +Inf
      for( l in 1:K ){
        nm = norm( as.matrix( DF[si,] - cents[l,] ), type="2" )
        if( nm < smallest_norm ){
          smallest_norm = nm
          new_labels[si] = l
        }
      }
    }

    # Part (e): Repeat until labels stop changing:
    # 
    if( sum( new_labels == labels ) == n ){
      break
    }else{
      labels = new_labels
    }
}

# Part (f): Plot the updated cluster labels
#
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_3_final_plot.eps", onefile=FALSE, horizontal=FALSE) }
plot( DF$x1, DF$x2, cex=2.0, pch=19, col=(labels+1), xlab="gene index", ylab="unpaired t-value" )
grid()
if( save_plots ){ dev.off() }
