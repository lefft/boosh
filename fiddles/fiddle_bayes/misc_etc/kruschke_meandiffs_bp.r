# code from kruschke blog post: 
#     http://doingbayesiandataanalysis.blogspot.com.au/2017/06/
#                      difference-of-means-for-paired-data.html
#



#----------------------------------------------------------------------------
# Jags-DifferenceOfPairedScores.R 
# John Kruschke, June 2017.
# For further info, see:
# Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition: 
# A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

#--------------------------------------------------------------------------
# Load the data:
#--------------------------------------------------------------------------

# For real research, you would read in your data file here. This script expects
# the data to have two columns for the paired scores, one row per pair. No
# missing data.
#
# myData = read.csv("YourDataFileName.csv") 
# y = myData[,c("ColumnNameOfPreScore","ColumnNameOfPostScore")]

# Generate simulated data:
library(MASS)
mu1 = 100.0
sigma1 = 15.0
mu2 = 108.0
sigma2 = 20.0
rho = 0.9
Sigma <- matrix(c( sigma1^2 , rho*sigma1*sigma2 ,
                   rho*sigma2*sigma1 , sigma2^2 ) , ncol=2 , byrow=TRUE )
set.seed(47405)
N = 100
yMat = mvrnorm(n=N, mu=c(mu1,mu2), Sigma=Sigma, empirical=TRUE)
colnames(yMat) = c("Y1","Y2")
write.csv( data.frame(yMat) , file="Jags-DifferenceOfPairedScores-Data.csv" )
 
# Now read in simulated data as if it were a data file:
myData = read.csv("Jags-DifferenceOfPairedScores-Data.csv")
y = myData[,c("Y1","Y2")]

#----------------------------------------------------------------------------
# The rest can remain unchanged, except for the specification of difference of
# means at the very end.
#
# The script below is a modification of the script Jags-MultivariateNormal.R,
# and there are some vestigial and unnecessary complexities. The previous script
# involved data with possibly more than two "y" columns, whereas the present
# script is only concerned with two "y" columns.
#----------------------------------------------------------------------------

# Load some functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
# Install the ellipse package if not already:
want = c("ellipse")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }

# Standardize the data:
sdOrig = apply(y,2,sd)
meanOrig = apply(y,2,mean)
zy = apply(y,2,function(yVec){(yVec-mean(yVec))/sd(yVec)})
# Assemble data for sending to JAGS:
dataList = list(
  zy = zy ,
  Ntotal =  nrow(zy) ,
  Nvar = ncol(zy) ,
  # Include original data info for transforming to original scale:
  sdOrig = sdOrig ,
  meanOrig = meanOrig ,
  # For wishart (dwish) prior on inverse covariance matrix:
  zRscal = ncol(zy) ,  # for dwish prior
  zRmat = diag(x=1,nrow=ncol(zy))  # Rmat = diag(apply(y,2,var))
)

# Define the model:
modelString = "
model {
  for ( i in 1:Ntotal ) {
    zy[i,1:Nvar] ~ dmnorm( zMu[1:Nvar] , zInvCovMat[1:Nvar,1:Nvar] ) 
  }
  for ( varIdx in 1:Nvar ) { zMu[varIdx] ~ dnorm( 0 , 1/2^2 ) }
  zInvCovMat ~ dwish( zRmat[1:Nvar,1:Nvar] , zRscal )
  # Convert invCovMat to sd and correlation:
  zCovMat <- inverse( zInvCovMat )
  for ( varIdx in 1:Nvar ) { zSigma[varIdx] <- sqrt(zCovMat[varIdx,varIdx]) }
  for ( varIdx1 in 1:Nvar ) { for ( varIdx2 in 1:Nvar ) {
    zRho[varIdx1,varIdx2] <- ( zCovMat[varIdx1,varIdx2] 
                               / (zSigma[varIdx1]*zSigma[varIdx2]) )
  } }
  # Convert to original scale:
  for ( varIdx in 1:Nvar ) { 
    sigma[varIdx] <- zSigma[varIdx] * sdOrig[varIdx] 
    mu[varIdx] <- zMu[varIdx] * sdOrig[varIdx] + meanOrig[varIdx]
  }
  for ( varIdx1 in 1:Nvar ) { for ( varIdx2 in 1:Nvar ) {
    rho[varIdx1,varIdx2] <- zRho[varIdx1,varIdx2]
  } }
}
" # close quote for modelString
writeLines( modelString , con="Jags-MultivariateNormal-model.txt" )

# Run the chains:
nChain = 3
nAdapt = 500
nBurnIn = 500
nThin = 10
nStepToSave = 20000
require(rjags)
jagsModel = jags.model( file="Jags-MultivariateNormal-model.txt" , 
                        data=dataList , n.chains=nChain , n.adapt=nAdapt )
update( jagsModel , n.iter=nBurnIn )
codaSamples = coda.samples( jagsModel , 
                            variable.names=c("mu","sigma","rho") ,
                            n.iter=nStepToSave/nChain*nThin , thin=nThin )

# Convergence diagnostics:
parameterNames = varnames(codaSamples) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=codaSamples , parName=parName )
}

# Examine the posterior distribution:
mcmcMat = as.matrix(codaSamples)
chainLength = nrow(mcmcMat)
Nvar = ncol(y)
# Create subsequence of steps through chain for plotting:
stepVec = floor(seq(1,chainLength,length=20)) 

# Make plots of posterior distribution:

# Preparation -- define useful functions:
library(ellipse)
expandRange = function( x , exMult=0.2 ) {
  lowVal = min(x)
  highVal = max(x)
  wid = max(x)-min(x)
  return( c( lowVal - exMult*wid , highVal + exMult*wid ) )
}

for ( varIdx in 1:Nvar ) {
  openGraph(width=7,height=3.5)
  par( mar=c(3.5,3,2,1) , mgp=c(2.0,0.7,0) )
  layout(matrix(1:2,nrow=1))
  # Marginal posterior on means:
  plotPost( mcmcMat[ , paste0("mu[",varIdx,"]") ] ,
            xlab=paste0("mu[",varIdx,"]") ,
            main=paste( "Mean of" , colnames(y)[varIdx] ) )
  # Marginal posterior on standard deviations:
  plotPost( mcmcMat[ , paste0("sigma[",varIdx,"]") ] ,
            xlab=paste0("sigma[",varIdx,"]") ,
            main=paste( "SD of" , colnames(y)[varIdx] ) )
}

for ( varIdx1 in 1:(Nvar-1) ) {
  for ( varIdx2 in (varIdx1+1):Nvar ) {
    openGraph(width=7,height=3.5)
    par( mar=c(3.5,3,2,1) , mgp=c(2.0,0.7,0) )
    layout(matrix(1:2,nrow=1))
    # Marginal posterior on correlation coefficient
    plotPost( mcmcMat[ , paste0("rho[",varIdx1,",",varIdx2,"]") ] ,
              xlab=paste0("rho[",varIdx1,",",varIdx2,"]") ,
              main=paste( "Corr. of" , colnames(y)[varIdx1] , 
                          "and" , colnames(y)[varIdx2] ) )
    # Data with posterior ellipse
    ellipseLevel = 0.90
    plot( y[,c(varIdx1,varIdx2)] , asp=1 , # pch=19 , 
          xlim=expandRange(y[,varIdx1],0.1) , 
          ylim=expandRange(y[,varIdx2],0.1) ,
          xlab=colnames(y)[varIdx1] , ylab=colnames(y)[varIdx2] ,
          main=bquote("Data with posterior "*.(ellipseLevel)*" level contour") )
    abline(0,1,lty="dashed")
    # Posterior ellipses:
    for ( stepIdx in stepVec ) {
      points( ellipse( mcmcMat[ stepIdx , 
                                paste0("rho[",varIdx1,",",varIdx2,"]") ] , 
                       scale=mcmcMat[ stepIdx , 
                                      c( paste0("sigma[",varIdx1,"]") ,
                                         paste0("sigma[",varIdx2,"]") ) ] , 
                       centre=mcmcMat[ stepIdx ,
                                       c( paste0("mu[",varIdx1,"]") ,
                                          paste0("mu[",varIdx2,"]") ) ] , 
                       level=ellipseLevel ) , 
              type="l" , col="skyblue" , lwd=1 )
    }
    # replot data:
    points( y[,c(varIdx1,varIdx2)] )
    points( mean(y[,varIdx1]) , mean(y[,varIdx2]) , pch="+" , col="red" , cex=2 )
  }
}

# Show data descriptives on console:
cor( y )
apply(y,2,mean)
apply(y,2,sd)

#-----------------------------------------------------------------------------
# Difference of means. N.B.: THIS ONLY MAKES SENSE IF THE MEANS ARE ON THE SAME 
# SCALE, E.G., BEFORE-TREATMENT AND AFTER-TREATMENT SCORES ON THE SAME MEASURE.
# Change the specification of indices as appropriate to your variable names and
# interests.
#-----------------------------------------------------------------------------
# Specify indices of desired means:
MuIdx1 = which(colnames(y)=="Y2")
MuIdx2 = which(colnames(y)=="Y1")
# Make plot of posterior difference:
openGraph(height=3.5,width=4)
par( mar=c(3.5,1,2,1) , mgp=c(2.0,0.7,0) )
plotPost( mcmcMat[ , paste0("mu[",MuIdx1,"]") ]
          - mcmcMat[ , paste0("mu[",MuIdx2,"]") ],
          xlab=paste0( "mu[",MuIdx1,"] - mu[",MuIdx2,"]" ) ,
          main=bquote( mu[ .(colnames(y)[MuIdx1]) ]
                       - mu[ .(colnames(y)[MuIdx2]) ] ) ,
          cex.main=1.5 , ROPE=c(-1.5,1.5) ) 
points( mean(y[,MuIdx1])-mean(y[,MuIdx2]) , 0 , pch="+" , col="red" , cex=2 )

#---------------------------------------------------------------------------

# Re-do using BEST package:
library(BEST)
BESTout = BESTmcmc( y[,MuIdx1] - y[,MuIdx2] , 
                    numSavedSteps=nStepToSave , thinSteps=nThin )
openGraph(height=3.5,width=4)
par( mar=c(3.5,1,2,1) , mgp=c(2.0,0.7,0) )
plot( BESTout , compVal=0.0 , ROPE=c(-1.5,1.5) , 
      xlab=paste0( "mu[",MuIdx1,"] - mu[",MuIdx2,"]" ) , 
      main=bquote( mu[ .(colnames(y)[MuIdx1]) ]
                   - mu[ .(colnames(y)[MuIdx2]) ] ) )

#---------------------------------------------------------------------------