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
# EPage 431
#
#-----

save_plots = F

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a-b):
# 
hclust.complete = hclust( dist(USArrests), method="complete" )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_9_complete_linkage.eps", onefile=FALSE, horizontal=FALSE) }
plot( hclust.complete, xlab="", sub="", cex=0.9 )
if( save_plots ){ dev.off() }

#cutree( hclust.complete, h=150 ) # height we cut at 
ct = cutree( hclust.complete, k=3 ) # number of clusters to cut into

# Print which states go into each cluster:
# 
for( k in 1:3 ){
  print(k)
  print( rownames( USArrests )[ ct == k ] )
}


# Part (c-d):
# 
hclust.complete.scale = hclust( dist(scale(USArrests,center=FALSE)), method="complete" )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter10/prob_9_complete_linkage_w_scaling.eps", onefile=FALSE, horizontal=FALSE) }
plot( hclust.complete.scale, xlab="", sub="", cex=0.9 )
if( save_plots ){ dev.off() }

ct = cutree( hclust.complete.scale, k=3 ) # number of clusters to cut into

# Print which states go into each cluster in this case:
# 
for( k in 1:3 ){
  print(k)
  print( rownames( USArrests )[ ct == k ] )
}

