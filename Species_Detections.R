#############################################################
# Species_Detections.R
# created by Joanna Burgar, 26-Jan-2017
# script for determining optimal sampling occassions per species/year
#############################################################

# set directory
setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/")
DayLookup <- read.csv("StudyDays.csv")
CamXY <- read.csv("CamXY.csv",row.names=1)
traplocs <- as.matrix(CamXY[, 2:3])           # 62 camera locations in this matrix


#############################################################
#############################################################
#############################################################
###--- Black Bear
BlkBearMatrix <- read.csv("BBMatrix.csv",header=T, row.names=1) ## import the BlkBear matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(BlkBearMatrix[,184:376],na.rm=T)  #217 - 2012 detections between 22-April and 31-Oct
sum(is.na(BlkBearMatrix[,184:376])) # 461
#BlkBear2012 <- BlkBearMatrix[,184:376]
#BB2012 <- BlkBear2012[apply(BlkBear2012,1,function(x)!any(is.na(x))), , drop=F]
#sum(is.na(BB2012)) # no NAs
#sum(BB2012) # 214 detections
#dim(BB2012) # 57 193
#complete.cases(BlkBearMatrix[,184:376]) ## rows 32,33,55,58,62
#62-5 #57 cameras
#plot(apply(BB2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)
#sum(BB2012[,75:167]) # 154 detections
#167+184 #351
#75+184 #259
#DayLookup[which(DayLookup$StudyDay==259),] # 6-Jul-2012
#DayLookup[which(DayLookup$StudyDay==351),] # 6-Oct-2012
## 2012 best bear detections for 3 months Study days 259:351;

sum(BlkBearMatrix[,259:351],na.rm=T)  #157 - 2012 detections between 6-Jul and 6-Oct
BlkBear2012 <- BlkBearMatrix[,259:351]
BB2012 <- BlkBear2012[apply(BlkBear2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(BB2012)) # no NAs
sum(BB2012) # 157 detections
dim(BB2012) # 59 93
complete.cases(BlkBearMatrix[,259:351]) ## rows 32,55,62
62-3 #59 cameras

n.BB2012<- as.matrix(BB2012)
n <- n.BB2012

###-- map of 2012 detections
y.BB <- rowSums(BlkBear2012[-1])
notna.BB <- !is.na(y.BB)
y.BB <- y.BB[notna.BB]
locs <- traplocs[notna.BB,c("UTME","UTMN")]
sz <- y.BB/max(y.BB)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/BlkBearDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Bear Detections 6-July to 6-Oct 2012")
spatial.plot(traplocs[notna.BB,c("UTME","UTMN")],y.BB,cx=1+sz*6,add=TRUE)
dev.off()


###--- 2013
#sum(BlkBearMatrix[,498:761],na.rm=T)  #195 - 2013 detections between 02-Mar and 20-Nov
sum(BlkBearMatrix[,624:716],na.rm=T)  #139 - 2013 detections between 06-Jul and 06-Oct


sum(is.na(BlkBearMatrix[,624:716])) #319
BlkBear2013 <- BlkBearMatrix[,624:716]
BB2013 <- BlkBear2013[apply(BlkBear2013,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(BB2013)) # no NAs
sum(BB2013) # 124 detections
dim(BB2013) # 56 93
complete.cases(BlkBearMatrix[,624:716]) #22,27,32,33,43,48
62-6 #56 cameras

plot(apply(BB2013,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)
colSums(BB2013)
624-498

#DayLookup[which(DayLookup$StudyDay==624),] # 6-Jul-2013
#DayLookup[which(DayLookup$Date=="06-Jul-13"),] # StudyDay 624
#DayLookup[which(DayLookup$Date=="06-Oct-13"),] # StudyDay 716

###-- map of 2013 detections
y.BB <- rowSums(BlkBear2013[-1])
notna.BB <- !is.na(y.BB)
y.BB <- y.BB[notna.BB]
locs <- traplocs[notna.BB,c("UTME","UTMN")]
sz <- y.BB/max(y.BB)
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Bear Detections 6-Jul to 6-Oct 2013")
spatial.plot(traplocs[notna.BB,c("UTME","UTMN")],y.BB,cx=1+sz*6,add=TRUE)


###--- 2014
sum(BlkBearMatrix[,989:1081],na.rm=T) #107 - 2014 detections between 24-April and 02-Oct
sum(is.na(BlkBearMatrix[,989:1081])) #1838
BlkBear2014 <- BlkBearMatrix[,989:1081]
BB2014 <- BlkBear2014[apply(BlkBear2014,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(BB2014)) # no NAs
sum(BB2014) # 100 detections
dim(BB2014) # 50 93
complete.cases(BlkBearMatrix[,989:1081])#2, 8,11,23,27,31,32,33,44,47,53,59
62-12 #50 cameras
#DayLookup[which(DayLookup$Date=="06-Jul-14"),] # StudyDay 989
#DayLookup[which(DayLookup$Date=="06-Oct-14"),] # StudyDay 1081

###-- map of 2014 detections
y.BB <- rowSums(BlkBear2014[-1])
notna.BB <- !is.na(y.BB)
y.BB <- y.BB[notna.BB]
locs <- traplocs[notna.BB,c("UTME","UTMN")]
sz <- y.BB/max(y.BB)
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Bear Detections 6-Jul to 6-Oct 2014")
spatial.plot(traplocs[notna.BB,c("UTME","UTMN")],y.BB,cx=1+sz*6,add=TRUE)

#############################################################
#############################################################
#############################################################
###--- Caribou
CaribouMatrix <- read.csv("Caribou_Matrix.csv",header=T, row.names=1) ## import the BlkBear matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(CaribouMatrix[,1:1101],na.rm=T)  #52 independent detection events over entire study
sum(is.na(CaribouMatrix[,1:1101])) # 7325 NA

hist(colSums(CaribouMatrix[,1:1101],na.rm=T))
plot(colSums(CaribouMatrix[,1:1101],na.rm=T))
plot(colSums(CaribouMatrix[,100:350],na.rm=T))
plot(colSums(CaribouMatrix[,163:255],na.rm=T))
plot(colSums(CaribouMatrix[,177:268],na.rm=T))

sum(CaribouMatrix[,163:253],na.rm=T) #14 detections over 90 days
sum(CaribouMatrix[,177:268],na.rm=T) #14 detections over 90 days

DayLookup[which(DayLookup$StudyDay==163),] # 1-Apr-2012
DayLookup[which(DayLookup$StudyDay==177),] # 15-Apr-2012
DayLookup[which(DayLookup$StudyDay==253),] # 30-Jun-2012
DayLookup[which(DayLookup$StudyDay==268),] # 15-Jul-2012

Caribou2012 <- CaribouMatrix[,163:253]

CA2012 <- Caribou2012[apply(Caribou2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(CA2012)) # no NAs
sum(CA2012) # 14 detections
dim(CA2012) # 59 91
complete.cases(CaribouMatrix[,163:253]) ## rows 32,58,62
62-3 #59 cameras
plot(apply(CA2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)

## 2012 best Caribou detections for 3 months summer ("snow-free"), during RAP, Study days 163:253;

n <- as.matrix(CA2012)

###-- map of 2012 detections
library(scrbook)
y.CA <- rowSums(CA2012[-1])
notna.CA <- !is.na(y.CA)
y.CA <- y.CA[notna.CA]
locs <- traplocs[notna.CA,c("UTME","UTMN")]
sz <- y.CA/max(y.CA)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/CaribouDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Caribou Detections 1-April to 30-June 2012")
spatial.plot(traplocs[notna.CA,c("UTME","UTMN")],y.CA,cx=1+sz*6,add=TRUE)
dev.off()


#############################################################
#############################################################
#############################################################
###--- Coyote
CoyoteMatrix <- read.csv("Coyote_Matrix.csv",header=T, row.names=1) ## import the Coyote matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(CoyoteMatrix[,1:1101],na.rm=T)  #837 independent detection events over entire study
sum(is.na(CoyoteMatrix[,1:1101])) # 7325 NA

hist(colSums(CoyoteMatrix[,1:1101],na.rm=T))
plot(colSums(CoyoteMatrix[,1:1101],na.rm=T))
plot(colSums(CoyoteMatrix[,200:415],na.rm=T))
plot(colSums(CoyoteMatrix[,215:415],na.rm=T))
plot(colSums(CoyoteMatrix[,255:345],na.rm=T))

sum(CoyoteMatrix[,215:415],na.rm=T) #239 detections over 200 days
sum(CoyoteMatrix[,215:305],na.rm=T) #84 detections over 90 days
sum(CoyoteMatrix[,255:345],na.rm=T) #109 detections over 90 days
sum(CoyoteMatrix[,325:415],na.rm=T) #113 detections over 90 days

DayLookup[which(DayLookup$StudyDay==215),] # 23-May-2012
DayLookup[which(DayLookup$StudyDay==254),] # 1-Jul-2012
DayLookup[which(DayLookup$StudyDay==305),] # 21-Aug-2012
DayLookup[which(DayLookup$StudyDay==325),] # 10-Sep-2012
DayLookup[which(DayLookup$StudyDay==345),] # 30-Sep-2012
DayLookup[which(DayLookup$StudyDay==415),] # 9-Dec-2012


Coyote2012 <- CoyoteMatrix[,254:345]

CO2012 <- Coyote2012[apply(Coyote2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(CO2012)) # no NAs
sum(CO2012) # 110 detections
dim(CO2012) # 59 92
complete.cases(CoyoteMatrix[,254:345]) ## rows 32,55,62
62-3 #59 cameras
plot(apply(CO2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)

## 2012 best coyote detections for 3 months summer ("snow-free") Study days 254:345;

n <- as.matrix(CO2012)

###-- map of 2012 detections
library(scrbook)
y.CO <- rowSums(CO2012[-1])
notna.CO <- !is.na(y.CO)
y.CO <- y.CO[notna.CO]
locs <- traplocs[notna.CO,c("UTME","UTMN")]
sz <- y.CO/max(y.CO)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/CoyoteDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Coyote Detections 1-July to 30-September 2012")
spatial.plot(traplocs[notna.CO,c("UTME","UTMN")],y.CO,cx=1+sz*6,add=TRUE)
dev.off()

#############################################################
#############################################################
#############################################################
###--- Lynx
LynxMatrix <- read.csv("Lynx_Matrix.csv",header=T, row.names=1) ## import the Lynx matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(LynxMatrix[,1:1101],na.rm=T)  #607 independent detection events over entire study
sum(is.na(LynxMatrix[,1:1101])) # 7325 NA

hist(colSums(LynxMatrix[,1:1101],na.rm=T))
plot(colSums(LynxMatrix[,1:1101],na.rm=T))
plot(colSums(LynxMatrix[,250:360],na.rm=T))

sum(LynxMatrix[,215:415],na.rm=T) #194 detections over 200 days
sum(LynxMatrix[,215:305],na.rm=T) #79 etections over 90 days
sum(LynxMatrix[,254:345],na.rm=T) #86 detections over 90 days
sum(LynxMatrix[,259:351],na.rm=T) #90 detections over 90 days
sum(LynxMatrix[,257:349],na.rm=T) #88 detections over 90 days
sum(LynxMatrix[,325:415],na.rm=T) #97 detections over 90 days

DayLookup[which(DayLookup$StudyDay==215),] # 23-May-2012
DayLookup[which(DayLookup$StudyDay==259),] # 6-Jul-2012
DayLookup[which(DayLookup$StudyDay==305),] # 21-Aug-2012
DayLookup[which(DayLookup$StudyDay==325),] # 10-Sep-2012
DayLookup[which(DayLookup$StudyDay==351),] # 6-Oct-2012
DayLookup[which(DayLookup$StudyDay==415),] # 9-Dec-2012

Lynx2012 <- LynxMatrix[,259:351]

LY2012 <- Lynx2012[apply(Lynx2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(LY2012)) # no NAs
sum(LY2012) # 90 detections
dim(LY2012) # 59 93
complete.cases(LynxMatrix[,259:351]) ## rows 32,55,62
62-3 #59 cameras
plot(apply(LY2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)

## 2012 best Lynx detections for 3 months summer ("snow-free") Study days 259:351;

n <- as.matrix(LY2012)

###-- map of 2012 detections
library(scrbook)
y.LY <- rowSums(LY2012[-1])
notna.LY <- !is.na(y.LY)
y.LY <- y.LY[notna.LY]
locs <- traplocs[notna.LY,c("UTME","UTMN")]
sz <- y.LY/max(y.LY)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/LynxDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Lynx Detections 6-July to 6-Octber 2012")
spatial.plot(traplocs[notna.LY,c("UTME","UTMN")],y.LY,cx=1+sz*6,add=TRUE)
dev.off()

#############################################################
#############################################################
#############################################################
###--- Moose
MooseMatrix <- read.csv("Moose_Matrix.csv",header=T, row.names=1) ## import the Moose matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(MooseMatrix[,1:1101],na.rm=T)  #81 independent detection events over entire study
sum(is.na(MooseMatrix[,1:1101])) # 7325 NA

hist(colSums(MooseMatrix[,1:1101],na.rm=T))
plot(colSums(MooseMatrix[,1:1101],na.rm=T))
plot(colSums(MooseMatrix[,217:309],na.rm=T))

sum(MooseMatrix[,215:415],na.rm=T) #30 detections over 200 days
sum(MooseMatrix[,217:309],na.rm=T) #20 etections over 90 days

DayLookup[which(DayLookup$StudyDay==217),] # 25-May-2012
DayLookup[which(DayLookup$StudyDay==309),] # 25-Aug-2012

Moose2012 <- MooseMatrix[,217:309]

MO2012 <- Moose2012[apply(Moose2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(MO2012)) # no NAs
sum(MO2012) # 20 detections
dim(MO2012) # 58 93
complete.cases(MooseMatrix[,217:309]) ## rows 32,55, 58,62
62-4 #59 cameras
plot(apply(MO2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "DaiMO Detections",cex.lab=1.4)

## 2012 best Moose detections for 3 months summer ("snow-free") Study days 217:309;

n <- as.matrix(MO2012)

###-- map of 2012 detections
library(scrbook)
y.MO <- rowSums(MO2012[-1])
notna.MO <- !is.na(y.MO)
y.MO <- y.MO[notna.MO]
locs <- traplocs[notna.MO,c("UTME","UTMN")]
sz <- y.MO/max(y.MO)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/MooseDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Moose Detections 25-May to 25-August 2012")
spatial.plot(traplocs[notna.MO,c("UTME","UTMN")],y.MO,cx=1+sz*6,add=TRUE)
dev.off()

#############################################################
#############################################################
#############################################################
###--- Wolf
WolfMatrix <- read.csv("Wolf_Matrix.csv",header=T, row.names=1) ## import the Wolf matrix (created in SpatialCountDensity_JBv2.R)

###--- 2012
sum(WolfMatrix[,1:1101],na.rm=T)  #501 independent detection events over entire study
sum(is.na(WolfMatrix[,1:1101])) # 7325 NA

hist(colSums(WolfMatrix[,1:1101],na.rm=T))
plot(colSums(WolfMatrix[,1:1101],na.rm=T))
plot(colSums(WolfMatrix[,214:380],na.rm=T))
plot(colSums(WolfMatrix[,214:380],na.rm=T))

sum(WolfMatrix[,200:400],na.rm=T) #115 detections over 200 days
sum(WolfMatrix[,214:306],na.rm=T) #64 etections over 90 days
sum(WolfMatrix[,254:345],na.rm=T) #64 etections over 90 days

DayLookup[which(DayLookup$StudyDay==214),] # 22-May-2012
DayLookup[which(DayLookup$StudyDay==254),] # 1-Jul-2012
DayLookup[which(DayLookup$StudyDay==345),] # 30-Sept-2012

Wolf2012 <- WolfMatrix[,254:345]

WO2012 <- Wolf2012[apply(Wolf2012,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(WO2012)) # no NAs
sum(WO2012) # 64 detections
dim(WO2012) # 59 92
complete.cases(WolfMatrix[,254:345]) ## rows 32,55,62
62-3 #59 cameras
plot(apply(WO2012,2,function(x) sum(x)), xlab= "Study Day", ylab= "DaiWO Detections",cex.lab=1.4)

## 2012 best Wolf detections for 3 WOnths summer ("snow-free") Study days 254:345, also consistent with coyote;

n <- as.matrix(WO2012)

###-- map of 2012 detections
library(scrbook)
y.WO <- rowSums(WO2012[-1])
notna.WO <- !is.na(y.WO)
y.WO <- y.WO[notna.WO]
locs <- traplocs[notna.WO,c("UTME","UTMN")]
sz <- y.WO/max(y.WO)

pdf(file="C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/WolfDetections2012.pdf")
par(mar=c(3,3,6,6))
plot(locs, pch="", xlim=c(range(locs[,-2])+c(-1000,1000)), ylim=c(range(locs[,-1])+c(-1000,1000)), main="Wolf Detections 1-July to 30-September 2012")
spatial.plot(traplocs[notna.WO,c("UTME","UTMN")],y.WO,cx=1+sz*6,add=TRUE)
dev.off()

