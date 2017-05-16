#############################################################
# WTD_scrUNT.R
# Chapter 18 of SCR book
# created by Joanna Burgar, 21-Dec-2016 - for WTD
# modified by Joanna Burgar, 16-May-2017 - for WTD
# script for estimating density of unmarked populations using scrUN update=TRUE
#############################################################
library(snowfall)
library(scrbook)

###--- set directory and load files
setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels")
DayLookup <- read.csv("StudyDays.csv")
WTDMatrix <- read.csv("WTD_Matrix.csv",header=T, row.names=1) ## import the WTD matrix (created in SpatialCountDensity_JBv2.R)
CamXY <- read.csv("CamXY.csv",row.names=1)

###--- White-tailed deer 2012 sampling occassion with most detections
DayLookup[which(DayLookup$Date=="01-Apr-12"),] # 163
DayLookup[which(DayLookup$Date=="30-Jun-12"),] # 253

sum(WTDMatrix[,259:351],na.rm=T)  #2026 - 2012 detections between 6-Jul and 6-Oct
sum(WTDMatrix[,163:253],na.rm=T)  #1531 - 2012 detections between 01-Apr and 30-Jun

WTD2012 <- WTDMatrix[,259:351]
WTD2012 <- WTD2012[apply(WTD2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(WTD2012)) # no NAs
sum(WTD2012) # 2005 detections
dim(WTD2012) # 59 93
complete.cases(WTDMatrix[,259:351]) ## rows 32,55,62
62-3 #59 cameras

n <- as.matrix(WTD2012)
sum(n) # 2005 detections
###---- camera station coordinates
summary(CamXY)

# specify how much to buffer sites by (in 10km units, according to coord.scale)
coord.scale <- 10000
buffer <- 1 # 10 km unit buffer

traplocs <- as.matrix(CamXY[, 2:3])           # 62 camera locations in this matrix
X <- traplocs/coord.scale                     # 62 trap coords in 10 km units
X=X[-62,]
X=X[-55,]
X=X[-32,]
dim(X)

###--- create xlims and ylims
Xl <- min(X[, 1] - buffer)
Xu <- max(X[, 1] + buffer)

Yl <- min(X[, 2] - buffer)
Yu <- max(X[, 2] + buffer)
xlims <- c(Xl,Xu); ylims <- c(Yl,Yu)  

area <- (Xu - Xl) * (Yu - Yl) # this is in units of 10 km * 10 km
areakm2 <- (Xu - Xl)*(Yu - Yl)*100
areakm2 # 6483 km2

###--- create xlims and ylims of scaled coordinates
## page 155 of SCR book "While the scaling of the coordinate system is theoretically irrelevant,
## a poorly scaled coordinate system can produce Markov chains that mix poorly."
X.scaled <- X[,1]-min(X[,1])

Xl.scaled <- min(X.scaled - buffer)
Xu.scaled <- max(X.scaled + buffer)

Y.scaled <- X[,2]-min(X[,2])
Yl.scaled <- min(Y.scaled - buffer)
Yu.scaled <- max(Y.scaled + buffer)
xlims.scaled <- c(Xl.scaled,Xu.scaled); ylims.scaled <- c(Yl.scaled,Yu.scaled)  

areakm2.scaled <- (Xu.scaled - Xl.scaled)*(Yu.scaled - Yl.scaled)*100

X2 <- as.matrix(cbind(X.scaled,Y.scaled))
dim(X2) # scaled traploc matrix in 10 km units

###-- map of camera detections
#y.WTD <- rowSums(n)
#sz <- n/max(n)

#pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/WTDDetections2012.pdf")
#par(mar=c(3,3,6,8))
#plot(traplocs, pch="", xlim=c(range(traplocs[,-2])+c(-3000,3000)), ylim=c(range(traplocs[,-1])+c(-3000,3000)), main="White-tailed Deer Detections \nJuly 6 to October 6 2012")
#spatial.plot(traplocs,y.WTD,cx=1+sz*6,add=TRUE)
#dev.off()

