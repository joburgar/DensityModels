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

##---- load dataset with all Deer Project camera detections from Oct 2011 to Nov 2014 (updated in Feb 2015)
# (made in DeerCamAnalysis.R)

D <- read.csv("DeerCamAllDetectionsFeb2015.csv",header=T, row.names=1)
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
setwd("C://Users/JBurgar/Documents/R/Analysis/DensityEstimation/BorealDeerProject/DensityModels/")
DayLookup <- read.csv("StudyDays.csv")

##---- camera station coordinates
CamXY <- read.csv("CamXY.csv",row.names=1)
summary(CamXY)


##--- Create a subset of images for 6 key species (Wolf, bear, moose, caribou, coyote, lynx)
Dsub <- subset(D, D$Species=="blkbear"| D$Species=="caribou"| D$Species=="coyote"| D$Species=="lynx"| D$Species=="moose"| D$Species=="wolf")
head(Dsub)
Dsub$fSite <- as.factor(Dsub$Site)
D1 <- Dsub[order(Dsub$Species,Dsub$fSite, Dsub$Date.Time),]
summary(D1)
names(D1)

##--- Reduce to dataset with fewer variables
## does the Total varible reflect group size???
Ddt <- D1[c(1:3,5,8,10,23:26)]
head(Ddt)
summary(Ddt)
nrow(Ddt)


##--- Create "independent" detections with only 1 detection for sequences of species within 5 minutes elapsed time between successive images
##--- Code adapted from Sandry (Nancy)
D2 <-Ddt

names(D2)
D2min<-data.frame()

for(site in unique(D2$Site)){
  sitesub<-D2[D2$Site==site, ]
  
  for(species in unique(sitesub$Species)){
    speciessub<-sitesub[sitesub$Species==species, ]
    
    for (date in unique(speciessub$Date)){
      datesub<-speciessub[speciessub$Date==date, ]
      
      timepass <- c(60)                                                       # length of time in seconds
      for (i in 2:length(datesub$Date.Time)) {
        timepass <- c(timepass,datesub$Date.Time[i] - datesub$Date.Time[i-1]) 
      }
      
      D2min <- rbind(D2min,datesub[timepass > 29,])                           # time between images in 1 min intervals - c(60)
    }
  }
}

length(unique(D2min$Site))
length(unique(D2$Site))

nrow(D2) ## original dataframe has 10168 rows
nrow(D2min) ## aggregated dataframe has 3357 rows

names(D2min)

# add an event indicator of 1 for each of these hour-detections ("detection")
D2min$detection <- 1

##---- Create count matrix for species of interest
	# Site x Occasion matrix of count of detections, with NAs for occasions when site (camera) was not active

### Subset for particular species	
  # NOTE: should try to streamline the code so only species name needs to be changed 
    # (e.g. create function, or at least use standard variable names so not all need changing)

## just need to update D2min$Species=="" to run for other species

#Nsp <- D2min[which(D2min$Species=="blkbear"),]
#Nsp <- D2min[which(D2min$Species=="caribou"),]
#Nsp <- D2min[which(D2min$Species=="coyote"),]
#Nsp <- D2min[which(D2min$Species=="moose"),]
#Nsp <- D2min[which(D2min$Species=="lynx"),]
#Nsp <- D2min[which(D2min$Species=="wolf"),]

Nsp$Species <- factor(Nsp$Species)

with(Nsp, table(StudyDay, detection))
with(Nsp, table(Site, detection))

length(unique(Nsp$StudyDay)) # 307 days with bear detections; 46 caribou; 500 coyote; 73 moose; 408 lynx; 345 wolf
sum(Nsp$detection) # 605, the total number of "indepdendent" detections; 52 caribou; 837 coyote; 82 moose; 648 lynx; 501 wolf


### use reshape2 package to create matrix of Site x StudyDay, with value being number of "indepdendent" detections
  # currently independent is set to >5 min between elapsed images
  # note that this could change if we change the count (event) definition
  # this value could be considered both a Poisson (count) variable
  # [or a binomial (trial) variable (X of 24 hours) - what would it be now that the event is per "indepdendent" detections?]

  # useful reshape suggestions here: http://stackoverflow.com/questions/9617348/reshape-three-column-data-frame-to-matrix

library(reshape2)

# sum the number of independent-detections for each Site on each StudyDay (length would be same since Detection = 1)
  # this creates dataframe, so Site is first column
  # only includes sites and days with species detections

Sp.tmp <- dcast(Nsp,Site~StudyDay,fun.aggregate = sum, value.var="detection",fill=0) 
dim(Sp.tmp) ## 58 sites for 307 study days (bear); 12 for 47 (caribou); 58 for 501 (coyote); 38 for 74 (moose)
            ## 60 for 409 (lynx); 57 for 346 (wolf)

###--- need to add remaining sites with non-detections
# specify Sites as rownames and drop Site column
row.names(Sp.tmp) <- Sp.tmp$Site
Sp.tmp <- Sp.tmp[,-1]

# create SitexDay matrix of 0's for days and sites missing from species detections 
  # to provide for complete coverage including days with no species detections
  # identify which of study days are not in species dataframe
study.days <- 1:max(D$StudyDay)
Sp.missing <- study.days[-which(study.days %in% names(Sp.tmp))]
Sp.zeros <- as.data.frame.matrix(matrix(0,nrow=length(unique(D$Site)),ncol=length(Sp.missing)))
names(Sp.zeros)<- as.character(Sp.missing) # specify correct StudyDay column names
row.names(Sp.zeros) <- unique(D$Site) # specify correct Site names

# add all 0 rows for sites missing from Species detection dataframe
  # make sure StudyDay (column) and Site (row) names are correctly specified
Sp.tmp0 <- as.data.frame(matrix(0,ncol = length(Sp.tmp), nrow = nrow(Sp.zeros) - nrow(Sp.tmp)))
names(Sp.tmp0) <- names(Sp.tmp)
row.names(Sp.tmp0) <- row.names(Sp.zeros)[-which(row.names(Sp.zeros) %in% row.names(Sp.tmp))]
Sp.tmp2 <- rbind(Sp.tmp,Sp.tmp0)
  # sort rows correctly
Sp.tmp2 <- Sp.tmp2[order(as.numeric(row.names(Sp.tmp2))),]
dim(Sp.tmp2) ## 63 sites for 307 study days (bear); 63 for 46 (caribou); 63 for 500 (coyote); 63 for 73 (moose)
              # 63 for 407 (lynx); 63 for 345 (wolf)

# merge the detection and zero columns (dataframes) and sort by study day
Sptmp <- data.frame(Sp.tmp2,Sp.zeros,check.names=FALSE)
Sp.mat <- Sptmp[,order(as.numeric(names(Sptmp)))]

# check sum of hour detections (against earlier sum for species, above)
sum(Sp.mat) # 605 (bear), 52 (caribou), 837 (coyote) 82 (moose); 647 (lynx - missing one?); 501 (wolf)

# drop Site 64 (row 63) since malfunctioned and lacked data (could do this earlier in code above) - is this for all species?
Sp.mat <- Sp.mat[-63,]
dim(Sp.mat)
### Sp.mat is now a dataframe of 62 sites x 1101 days, with values representing count of indepdendent-detections each day

### Now need to correct for days on which individual camera stations were not active 

# Import matrix of camera sampling effort, indicating inactive days
  # 1 = active, 0 = inactive

effort <- read.csv("CameraTrapEffort.csv",row.names=1,check.names = FALSE)

# Now replace values in species count matrix with NAs for inactive days
  # first rename species matrix in case it doesn't work properly
SpMatrix <- Sp.mat

  # likely a more efficient way than using a For loop!

for (SITE in 1:nrow(SpMatrix)) {
  for (DAY in 1:ncol(SpMatrix)) {
    SpMatrix[SITE,DAY] <- ifelse(effort[SITE,DAY] == 0, NA, SpMatrix[SITE,DAY])
      } }


sum(SpMatrix,na.rm=T) # 604, lost 1 detection at site 45 (removed due to tree obstruction, see previous notes)
                      # 52 - no losses for caribou
                      # 834 - 3 losses for coyote
                      # 81 - 1 loss for moose
                      # 607 - 40 (41?) losses for lynx
                      # 501 - no losses for wolf

### SpMatrix is the dataframe to use going forward - just need to change species at start of code and can rerun for all other species
  # to have multiple matrixes in this workspace, rename for each species at this point

#BBMatrix <- SpMatrix
#write.csv(BBMatrix,"BBMatrix.csv")

#CBMatrix <- SpMatrix
#write.csv(CBMatrix,"Caribou_Matrix.csv")

#COMatrix <- SpMatrix
#write.csv(COMatrix,"Coyote_Matrix.csv")

#MOMatrix <- SpMatrix
#write.csv(MOMatrix,"Moose_Matrix.csv")

#LYMatrix <- SpMatrix
#write.csv(LYMatrix,"Lynx_Matrix.csv")

#WOMatrix <- SpMatrix
#write.csv(WOMatrix,"Wolf_Matrix.csv")

###--- Need to check the data - does it include groups? Now treats summation of images within 5 min as "independent" events, does not include group size

#SpMatrix <- read.csv("BBMatrix.csv",header=T, row.names=1)
#SpMatrix <- read.csv("Caribou_Matrix.csv",header=T, row.names=1)
#SpMatrix <- read.csv("Coyote_Matrix.csv",header=T, row.names=1)
#SpMatrix <- read.csv("Moose_Matrix.csv",header=T, row.names=1)
#SpMatrix <- read.csv("Lynx_Matrix.csv",header=T, row.names=1)
#SpMatrix <- read.csv("Wolf_Matrix.csv",header=T, row.names=1)

# some summaries on SpMatrix
rowSums(SpMatrix,na.rm=T)
sum(SpMatrix,na.rm=T) # total count = 604 (Bear); 52 (caribou); 834 (coyote); 81 (moose); 601 (lynx); 501 (wolf)
dotchart(rowSums(SpMatrix,na.rm=T),pch=16,ylab="Camera Station",xlab="Total Species Count")
barplot(sort(rowSums(SpMatrix,na.rm=T)))

# TRAP DAYS
sum(!is.na(SpMatrix)) # total = 60,937
apply(SpMatrix,1,function(x) sum(!is.na(x)))
summary(apply(SpMatrix,1,function(x) sum(!is.na(x))))
plot(apply(SpMatrix,1,function(x) sum(!is.na(x))))
dotchart(apply(SpMatrix,1,function(x) sum(!is.na(x))),cex=0.7,pch=16)
  # may want to drop some stations like 33 (only 93 trap days), 34 (358 trap days)
dotchart(apply(SpMatrix[-c(32,33),],1,function(x) sum(!is.na(x))),cex=0.7,pch=16)

# ACTIVE CAMERAS PER DAY
plot(apply(SpMatrix,2,function(x) sum(!is.na(x))), xlab= "Study Day", ylab= "Active Camera Stations",cex.lab=1.4)
  # add lines for Jan 1 and Jul 1 each year
abline("v"= c(72,254,438,619,803,984),col=c("blue","red"))
summary(apply(SpMatrix,2,function(x) sum(!is.na(x))))

# there is a good stretch with 60 cams from 27-Jan-12 (98) to 5-Jun-12 (228), but not good for deer because goes through high mortality and fawning
# period around AESRD survey in Jan 2013 might be ok to start
DayLookup[which(DayLookup$Date=="01-Dec-12"),]
DayLookup[which(DayLookup$StudyDay==1322),]
apply(SpMatrix,2,function(x) sum(!is.na(x)))[407:463]

summary(DayLookup)

## 3 bear "seasons" - 22-Apr-12 to 31-Oct-12; 02-Mar-13 to 20-Nov-13; 24-Apr-14 to 02-Oct-14
## how to truncate the data? By first/last bear detection per season or by time of year (e.g. 1 March to 30 Nov)?

DayLookup[which(DayLookup$StudyDay==184),] # first bear detection on 22-Apr-12
DayLookup[which(DayLookup$StudyDay==376),] # no bear detections between 31-Oct-12 and 02-Mar-13
DayLookup[which(DayLookup$StudyDay==498),]
DayLookup[which(DayLookup$StudyDay==761),] # no bear detections between 20-Nov-13 and 24-Apr-14
DayLookup[which(DayLookup$StudyDay==916),]
DayLookup[which(DayLookup$StudyDay==1077),] # last bear detection 02-Oct-14

## for bear, use by time of year and only run for first year
DayLookup[which(DayLookup$Date=="01-Mar-12"),] # StudyDay 132
DayLookup[which(DayLookup$Date=="30-Nov-12"),] # StudyDay 406

DayLookup[which(DayLookup$Date=="01-Mar-13"),] # StudyDay 497
DayLookup[which(DayLookup$Date=="30-Nov-13"),] # StudyDay 771

DayLookup[which(DayLookup$Date=="01-Mar-14"),] # StudyDay 862
DayLookup[which(DayLookup$Date=="30-Nov-14"),] # StudyDay 1136

DayLookup[which(DayLookup$Date=="01-Jun-12"),] # StudyDay 224

# ACTIVE CAMERAS PER DAY
plot(apply(SpMatrix,2,function(x) sum(!is.na(x))), xlab= "Study Day", ylab= "Active Camera Stations",cex.lab=1.4)
# add lines for Mar 1 and Nov 30 each year
abline("v"= c(132,406,497,771, 862,1136),col=c("blue","red"))
summary(apply(SpMatrix,2,function(x) sum(!is.na(x))))

# add lines for caribou RAP
abline("v"= c(117,268,483,633,848,998),col=c("blue","red"))

### caribou timing periods
# period around AEP caribou restricted activity period
DayLookup[which(DayLookup$Date=="15-Feb-12"),] # start of RAP 2012 - 117
DayLookup[which(DayLookup$Date=="15-Jul-12"),] # end of RAP 2012 - 268

DayLookup[which(DayLookup$Date=="15-Feb-13"),] # start of RAP 2013 - 483
DayLookup[which(DayLookup$Date=="15-Jul-13"),] # end of RAP 2013 - 633

DayLookup[which(DayLookup$Date=="15-Feb-14"),] # start of RAP 2014 - 848
DayLookup[which(DayLookup$Date=="15-Jul-14"),] # end of RAP 2014 - 998

# add lines for moose RAP
abline("v"= c(86,192,452,557,817,922),col=c("blue","red"))

### moose timing periods
# period around AEP ungulate restricted activity period
DayLookup[which(DayLookup$Date=="15-Jan-12"),] # start of RAP 2012 - 86 
DayLookup[which(DayLookup$Date=="30-Apr-12"),] # start of RAP 2012 - 192

DayLookup[which(DayLookup$Date=="15-Jan-13"),] # start of RAP 2013 - 452 
DayLookup[which(DayLookup$Date=="30-Apr-13"),] # start of RAP 2013 - 557

DayLookup[which(DayLookup$Date=="15-Jan-14"),] # start of RAP 2014 - 817 
DayLookup[which(DayLookup$Date=="30-Apr-14"),] # start of RAP 2014 - 922

####----- SETTING UP DATA FOR SMR - PARTIALLY MARKED POPULATION DENSITY ESTIMATION ------####

##-- Detections of COLLARED individuals --## 

Dcollar_all <- D2[grep("collar",D2$CommentsImages, ignore.case = TRUE),]
table(Dcollar_all$Site, droplevels(Dcollar_all$Species))
#blkbear caribou    wolf 
#4       39      160 

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

summary(D2min)

SMR_wolf <- D2min[which(D2min$Species=="wolf"),]
head(SMR_wolf)

Dcollar <- D2min[grep("collar",D2min$CommentsImages, ignore.case = TRUE),]

table(droplevels(Dcollar$Species))
table(Dcollar$Site,droplevels(Dcollar$Species))
#blkbear caribou    wolf 
#1       9      27 

#duplicated(Dcollar[which(Dcollar$Species=="wolf"),]$Date.Time) # no duplicates
#names(Dcollar)

nrow(SMR_wolf)
tail(SMR_wolf[order(SMR_wolf$CommentsImages),])
tail(SMR_wolf[order(SMR_wolf$Site),])

SMR_wolf <- SMR_wolf[order(SMR_wolf$Site),]
SMR_wolf[400:445,1:6]
SMR_wolf[350:445,1:6]
