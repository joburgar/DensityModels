#############################################################
# SC_SMR_prep.R
# from Cole Burton as "SpatialCountDensity.R", started 3-Nov-2016 (updated 25-Nov-2016)
# revised by Joanna Burgar, 30-Nov-2016
# code to select "marked" individuals, added by Joanna Burgar, 7-Mar-2017
# script for estimating density of unmarked populations
# derived from DeerDensity.R (Boreal Deer Project)
#############################################################


# set directory
setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/")
###-------------------------------------------------###
#                                                     #
# 		Load and Organize Data 						              #
#                                                     #
###-------------------------------------------------###

###--- Load appropriate packages
library(plyr)		# for the "ddply" function
library(reshape2)	# for formatting data frames
library(dplyr)		# for applying functions to subsets of data frames
library(ggplot2)	# for data visualization
library(tidyr)		# for data formatting functions
library(knitr)		# for the "kable" function for formatting tables


####----- SETTING UP DATA FOR SMR - PARTIALLY MARKED POPULATION DENSITY ESTIMATION ------####
##---- load dataset with all Deer Project camera detections from Oct 2011 to Nov 2014 (updated in Feb 2015)
# (made in DeerCamAnalysis.R)

D <- read.csv("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DeerCamAllDetectionsFeb2015.csv",header=T, row.names=1)
summary(D)
nrow(D)
head(D)
str(D)

# Change species names to make them easier to work with (shorter, no spaces)
Sp.names <- c("blkbear","caribou","raven","cougar","coyote","fisher","grouse",
              "human","lynx","marmot","marten","moose","other","owl",
              "rdfox","rdsquir","otter","snhare","Ucanid","Udeer","Uspecies",
              "Uung","wtdeer","wolf")
levels(D$Species) <- Sp.names

# specify date formats and calculate trap-days
D$Date.Time <- paste(D$Date,D$Time,sep = " ")
D$Date.Time <- as.POSIXct(strptime(D$Date.Time, "%d-%b-%y %I:%M %p", tz="MST"))
summary(D$Date.Time)

# convert DateStart (but note that this doesn't have time set, so will treat as midnight)
D$Start <- (strptime(D$DateStart, "%d-%b-%y", tz="MST"))

# calculate a unique day for each day of study
# taking straight difference will include partial days, so is really 24-hour periods from time first camera set
# since there is no start time entered, this should work from hour 0, so should be same as calendar day
# using "floor" so it doesn't round up to next day
D$StudyDay <- floor(as.numeric(difftime(D$Date.Time,min(D$Start),units="days")))
summary(D$StudyDay)

# import a csv with the study days and corresponding dates
DayLookup <- read.csv("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/StudyDays.csv")

##---- camera station coordinates
CamXY <- read.csv("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/CamXY.csv",row.names=1)
summary(CamXY)


##--- Create a subset of images for 6 key species (Wolf, bear, moose, caribou, coyote, lynx)
Dsub <- subset(D, D$Species=="blkbear"| D$Species=="caribou"| D$Species=="coyote"| D$Species=="lynx"| D$Species=="moose"| D$Species=="wolf")
head(Dsub)
Dsub$fSite <- as.factor(Dsub$Site)
D1 <- Dsub[order(Dsub$Species,Dsub$fSite, Dsub$Date.Time),]
summary(D1)
names(D1)

##--- Reduce to dataset with fewer variables
Ddt <- D1[c(1:3,5,8,10,23,25,26)]
glimpse(Ddt)
summary(Ddt)
D2 <-Ddt

##-- Detections of COLLARED individuals --## 

Dcollar_all <- D2[grep("collar",D2$CommentsImages, ignore.case = TRUE),]
table(droplevels(Dcollar_all$Species))
#blkbear caribou    wolf 
#4       39      160 

table(Dcollar_all$Site, droplevels(Dcollar_all$Species))
#     blkbear caribou wolf
#3        0       7   11
#6        0       0    3
#7        0       0    1
#9        0       0    6
#13       0       0    1
#19       0       6    0
#22       0       4    0
#25       4       0    0
#30       0       0    2
#31       0       0    8
#33       0       8    0
#37       0       0    2
#51       0       0   16
#52       0       0   10
#53       0       0   25
#55       0       0    8
#56       0       0    1
#57       0       0   17
#60       0       0   35
#61       0       0   12
#62       0      14    0
#63       0       0    2



SMR_wolf <- Dcollar_all[which(Dcollar_all$Species=="wolf"),]

SMR_wolf <- SMR_wolf[order(SMR_wolf$Site, SMR_wolf$Date.Time),]
nrow(SMR_wolf)

head(SMR_wolf)
tail(SMR_wolf)
View(SMR_wolf)

glimpse(SMR_wolf)
SMR_wolf.sumSD <- SMR_wolf %>%
  group_by(Site,StudyDay) %>%
  summarise(count_camdet = length(File))

SMR_wolf.sumSD <- SMR_wolf.sumSD[order(SMR_wolf.sumSD$Site, SMR_wolf.sumSD$StudyDay),]
View(SMR_wolf.sumSD)
min(SMR_wolf.sumSD$StudyDay)
DayLookup[which(DayLookup$StudyDay==505),] # 09-Mar-13

max(SMR_wolf.sumSD$StudyDay)
DayLookup[which(DayLookup$StudyDay==1079),] # 04-Oct-14

View(SMR_wolf.sumSD_01Jul_31Sep2013)
SMR_wolf.sumSD_01Jul_31Sep2013 <- subset(SMR_wolf.sumSD,SMR_wolf.sumSD$StudyDay>619 & SMR_wolf.sumSD$StudyDay<710)
glimpse(SMR_wolf.sumSD_01Jul_31Sep2013)
unique(SMR_wolf.sumSD_01Jul_31Sep2013$Site) # 8 sites with collared wolves during overlap time 2013
unique(SMR_wolf.sumSD_01Jul_31Sep2013$StudyDay) # 10 dates with collared wolves during overlap time 2013
table(SMR_wolf.sumSD_01Jul_31Sep2013$Site,SMR_wolf.sumSD_01Jul_31Sep2013$StudyDay)

sum(WolfMatrix[,505:1079],na.rm=T) #254 detections over time period of radio collaring


sum(WolfMatrix[,254:345],na.rm=T) #115 detections over 91 days 2012 - used in SC models


tail(DayLookup)
DayLookup[which(DayLookup$StudyDay==1),] # 22-Oct-2011
DayLookup[which(DayLookup$StudyDay==1079),] # 4-Oct-2014

DayLookup[which(DayLookup$Date=="01-Jul-13"),] # 619
DayLookup[which(DayLookup$Date=="30-Sep-13"),] # 710
sum(WolfMatrix[,619:710],na.rm=T) #50 detections over 91 days 2013 - same dates as previous year used in SC models

DayLookup[which(DayLookup$Date=="01-Jul-14"),] # 984
DayLookup[which(DayLookup$Date=="30-Sep-14"),] # 1075
sum(WolfMatrix[,984:1075],na.rm=T) #39 detections over 91 days 2013 - same dates as previous year used in SC models

DayLookup[which(DayLookup$Date=="01-Nov-13"),] # 742
DayLookup[which(DayLookup$Date=="31-Jan-14"),] # 833
sum(WolfMatrix[,833:742],na.rm=T) #45 detections over 91 days 2013 - same dates as previous year used in SC models

833-742

wtel.matrix <- WolfMatrix[,619:710] #matrix of camera detections over same time period as collaring data

wtel.matrix <- wtel.matrix[apply(wtel.matrix,1,function(x)!any(is.na(x))), , drop=F]
sum(is.na(wtel.matrix)) # no NAs
sum(wtel.matrix) # 50 detections
dim(wtel.matrix) # 57 active cameras over 92 days
complete.cases(WolfMatrix[,619:710]) ## rows 22,32,33,43,48
62-5 #57 cameras
plot(apply(wtel.matrix,2,function(x) sum(x)), xlab= "Study Day", ylab= "Daily Detections",cex.lab=1.4)

###---- camera station coordinates
summary(CamXY)

# specify how much to buffer sites by (in 10km units, according to coord.scale)
coord.scale <- 10000
buffer <- 1 # 10 km unit buffer

traplocs <- as.matrix(CamXY[, 2:3])           # 62 camera locations in this matrix
X <- traplocs/coord.scale                     # 62 trap coords in 10 km units
dim(X)

###--- create xlims and ylims
Xl <- min(X[, 1] - buffer)
Xu <- max(X[, 1] + buffer)

Yl <- min(X[, 2] - buffer)
Yu <- max(X[, 2] + buffer)
xlims <- c(Xl,Xu); ylims <- c(Yl,Yu)  

area <- (Xu - Xl) * (Yu - Yl) # this is in units of 10 km * 10 km
areakm2 <- (Xu - Xl)*(Yu - Yl)*100



###--- Load in wolf telemetry points
wolf.tel <- read.table("file:///C:/Users/JBurgar/Documents/ArcGIS/Projects/BorealDeer/SMR_RICC_wolf_UTMzn12.txt",header = TRUE, sep = ",")
summary(wolf.tel)
str(wolf.tel)
glimpse(wolf.tel)
head(wolf.tel$date_time)

# create date_time in usable posix format
wolf.tel$date <- as.character(substr(wolf.tel$date_time, 1,7))
wolf.tel$time <- as.character(substr(wolf.tel$date_time, 9,13))

wolf.tel$Date_POSIX <- as.POSIXct(strptime(wolf.tel$date, format = "%d%b%y"))
wolf.tel$Time_POSIX <- as.POSIXct(strptime(wolf.tel$time, format = "%H:%M"))
wolf.tel$Time_POSIX <- strftime(wolf.tel$Time_POSIX, format= "%H:%M")

wolf.tel$dt_POSIX <- as.POSIXct(paste(wolf.tel$Date_POSIX, wolf.tel$Time_POSIX), format = "%Y-%m-%d %H:%M")
wolf.tel$Year <- as.character(substr(wolf.tel$dt_POSIX,1,4))


wolf.tel.sumY <- wolf.tel %>%
  group_by(Animal_id, Year) %>%
  summarise(count_telem = length(FID))

wolf.tel.sumD <- wolf.tel %>%
  group_by(Animal_id, Date_POSIX) %>%
  summarise(count_telem = length(FID))
table(wolf.tel.sumD$Animal_id, wolf.tel.sumD$count_telem)

View(wolf.tel.sumY)

###--- create subset of data to use - all dates that may overlap
wtel <- subset(wolf.tel, wolf.tel$dt_POSIX < "2014-11-26 23:59:00")
glimpse(wtel)

wtel.sumY <- wtel %>%
  group_by(Animal_id, Year) %>%
  summarise(count_telem = length(FID))

wtel.sumD <- wtel %>%
  group_by(Animal_id, Date_POSIX) %>%
  summarise(count_telem = length(FID))


###--- create subset of wolf telemetry data to use - 01-July to 31-Sep 2013 overlap
wtel2013 <- subset(wolf.tel, wolf.tel$Date_POSIX > "2013-06-30" & wolf.tel$Date_POSIX < "2013-10-01")

glimpse(wtel2013)

wtel2013.sum <- wtel2013 %>%
  group_by(Animal_id) %>%
  summarise(count_telem = length(FID))
# 18 animals with collars during overlap time 2013

###--- camdates - check with wtel2013 possibilities
DayLookup[which(DayLookup$StudyDay==625),] # 07-Jul-13
DayLookup[which(DayLookup$StudyDay==640),] # 22-Jul-13
DayLookup[which(DayLookup$StudyDay==641),] # 23-Jul-13
DayLookup[which(DayLookup$StudyDay==643),] # 25-Jul-13
DayLookup[which(DayLookup$StudyDay==653),] # 04-Aug-13
DayLookup[which(DayLookup$StudyDay==667),] # 08-Aug-13
DayLookup[which(DayLookup$StudyDay==679),] # 30-Aug-13
DayLookup[which(DayLookup$StudyDay==691),] # 11-Sep-13
DayLookup[which(DayLookup$StudyDay==698),] # 18-Sep-13
DayLookup[which(DayLookup$StudyDay==708),] # 28-Sep-13


###--- create a subset of wolf telemetry data within 10 km of traps (buffered area)
xlims; ylims

names(wtel2013)
wtel2013b <- wtel2013[c(1,2,4,11:13,15:17)]

glimpse(wtel2013b)

table(wtel2013b$Animal_id, wtel2013b$Sex)
unique(wtel2013b$Animal_id)
wtel2013b$x <- wtel2013b$UTME/coord.scale
wtel2013b$y <- wtel2013b$UTMN/coord.scale

wtel2013b <- subset(wtel2013b, wtel2013b$x>xlims[1] & wtel2013b$x<xlims[2] & wtel2013b$y>ylims[1] & wtel2013b$y<ylims[2])
min(wtel2013b$x)
min(wtel2013b$y)

max(wtel2013b$x)
max(wtel2013b$y)

summary(wtel2013b)
write.csv(wtel2013b, "wtel2013b.csv")

###--- find any overlap between camera detections and telemetry locations
SMR_wolf2013 <- subset(SMR_wolf, SMR_wolf$Date.Time > "2013-06-30 23:59:00" & SMR_wolf$Date.Time < "2013-10-01 00:01:00")
str(SMR_wolf)


SMR_wolf2013 <- SMR_wolf2013[order(SMR_wolf2013$Date.Time),]
SMR_wolf2013[,2:4]
nrow(SMR_wolf2013)

# 07-Jul-13 10:30 AM detection at Site 30
wtel2013b[which(wtel2013b$Date_POSIX=="2013-07-07" &
                  wtel2013b$x>(X[30,1]-3) &
                  wtel2013b$x<(X[60,1]+3) &
                  wtel2013b$y>(X[60,2]-3) &
                  wtel2013b$y<(X[60,2]+3)),]

#FID Animal_id Sex   UTME    UTMN Date_POSIX  time Time_POSIX            dt_POSIX       x        y
#54228 54227     W003F   M 499824 6177644 2013-07-07 05:03      05:03 2013-07-07 05:03:00 49.9824 617.7644
#54229 54228     W003F   M 497073 6175786 2013-07-07 17:02      17:02 2013-07-07 17:02:00 49.7073 617.5786

# likely W003F

# 22-Jul-13  8:24 AM detection at Site 60
wtel2013b[which(wtel2013b$Date_POSIX=="2013-07-22" &
                  wtel2013b$x>(X[60,1]-3) &
                  wtel2013b$x<(X[60,1]+3) &
                  wtel2013b$y>(X[60,2]-3) &
                  wtel2013b$y<(X[60,2]+3)),]

#FID Animal_id Sex   UTME    UTMN Date_POSIX  time Time_POSIX            dt_POSIX       x        y
#100537 100536     W012F   F 537816 6155415 2013-07-22 04:02      04:02 2013-07-22 04:02:00 53.7816 615.5415
#136603 136602     W013D   F 530713 6160154 2013-07-22 01:00      01:00 2013-07-22 01:00:00 53.0713 616.0154
# could be one of two animals...W012F, W013D


# 23-Jul-13 12:12 AM detection at Site  9
wtel2013b[which(wtel2013b$Date_POSIX=="2013-07-23"| wtel2013b$Date_POSIX=="2013-07-22" &
                  wtel2013b$x>(X[9,1]-3) &
                  wtel2013b$x<(X[9,1]+3) &
                  wtel2013b$y>(X[9,2]-3) &
                  wtel2013b$y<(X[9,2]+3)),]
# could be one of nine animals...W003D, W003F, W004D, W004F, W005D, W007D, W012F, W013D, W015D - 3km buffer

wtel2013b[which(wtel2013b$Date_POSIX=="2013-07-23"| wtel2013b$Date_POSIX=="2013-07-22" &
                  wtel2013b$x>(X[9,1]-1) &
                  wtel2013b$x<(X[9,1]+1) &
                  wtel2013b$y>(X[9,2]-1) &
                  wtel2013b$y<(X[9,2]+1)),]
# could still be one of nine animals... 

#25-Jul-13  9:53 AM detections at Site  51
wtel2013b[which(wtel2013b$Date_POSIX=="2013-07-25" &
                  wtel2013b$x>(X[51,1]-3.5) &
                  wtel2013b$x<(X[51,1]+3.5) &
                  wtel2013b$y>(X[51,2]-3.5) &
                  wtel2013b$y<(X[51,2]+3.5)),]

#FID Animal_id Sex   UTME    UTMN Date_POSIX  time Time_POSIX            dt_POSIX       x        y
#54246 54245     W003F   M 507760 6170005 2013-07-25 05:03      05:03 2013-07-25 05:03:00 50.7760 617.0005
#54247 54246     W003F   M 510356 6170945 2013-07-25 17:03      17:03 2013-07-25 17:03:00 51.0356 617.0945
# likely W003F
