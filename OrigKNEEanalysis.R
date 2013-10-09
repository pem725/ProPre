##########################################################################
## KNEE Study Analysis                                                  ##
##                                                                      ##
## By:  Patrick E. McKnight                                             ##
##      Co-PI and later changed to consultant                           ##
##      George Mason University                                         ## 
##      pem@alumni.nd.edu                                               ##
##                                                                      ##
## Study Conducted at the UofA                                          ##
##     Arthritis Center                                                 ##
##     2002-2007                                                        ##
##     Funded by Arthritis Found. and NIH                               ##
##                                                                      ##
## PI:     David Yocum later Scott Going                                ##
## co-PI:  Issidro Villaneuva                                           ##
##                                                                      ##
## Latest Revision Date:  09/08/08                                      ##
##                                                                      ##
##########################################################################

## used xls2csv.pl to generate the following csv files

#THREERM <- read.csv("./FinalData/3RM.txt",T)
ACLS <- read.csv("./FinalData/ACLS.csv",T)
ADMIN <- read.csv("./FinalData/ADMINASSESSMENT.csv",T)
ADMINGROUPS <- read.csv("./FinalData/ADMINSUBGROUP.csv",T)
AE <- read.csv("./FinalData/AE.csv",T)
AHI <- read.csv("./FinalData/AHI.csv",T)
ASSESSMENT <- read.csv("./FinalData/ASSESSMENT.csv",T)
CESD <- read.csv("./FinalData/CESD.csv",T)
CSQ <- read.csv("./FinalData/CSQ.csv",T)
## CSARAW <- read.csv("./FinalData/CSARAW.txt",T)
DEMO <- read.csv("./FinalData/DEMOGRAPHICS.csv",T)
ECON <- read.csv("./FinalData/ECONOMICS.csv",T)
EUROQOL <- read.csv("./FinalData/EUROQOL.csv",T)
EXERCISE <- read.csv("./FinalData/EXERCISE.csv",T)
EXRECAP <- read.csv("./FinalData/EXERCISERECAP.csv",T)
FUNCTTEST <- read.csv("./FinalData/FUNCTIONTEST.csv",T)
KLKNEEOA <- read.csv("./FinalData/KLKNEEOA.csv",T)
KNEESCREENING <- read.csv("./FinalData/KNEESCREENING.csv",T)
MEDS <- read.csv("./FinalData/MEDICATION.csv",T)
MOS <- read.csv("./FinalData/MOS.csv",T)
NPI <- read.csv("./FinalData/NPI.csv",T)
PANAS <- read.csv("./FinalData/PANAS.csv",T)
PEDOMETER <- read.csv("./FinalData/PEDOMETER.csv",T)
PERSON <- read.csv("./FinalData/PERSON.csv",T)
PTASSESSMENT <- read.csv("./FinalData/PTASSESSMENT.csv",T)
SELFE <- read.csv("./FinalData/SELFE.csv",T)
SELFMANSESSQA <- read.csv("./FinalData/SELFMANAGEMENTSESSIONQA.csv",T)
SELFMANSUMMARY <- read.csv("./FinalData/SELFMANAGEMENTSUMMARY.csv",T)
SF36r <- read.csv("./FinalData/SF36.csv",T)
## SF36s <- read.csv("./FinalData/SF36Scores11052007.txt",T) # These are the scores
STUDYSTATUS <- read.csv("./FinalData/STUDYSTATUS.csv",T)
VAS2 <- read.csv("./FinalData/VAS2.csv",T)
WOMAC <- read.csv("./FinalData/WOMAC.csv",T)

#AGE <- read.csv("./FinalData/AGE.txt",T)
#ADMINA <- read.csv("./FinalData/ADMIN.txt",T)
#SSTATUS <- read.csv("./FinalData/STUDYSTATUS.txt",T) # key to break the blind


#########################################################
## Preliminary requirements for the following analyses ##
#########################################################

source("/home/pem/files/PAT/Research/Code-current/MRES/MRES.R")
library(mice)
library(car)
library(gplots)
library(foreign)
library(sem)
library(nlme)
library(lme4)
library(eRm)

# THREERM <- idparse(THREERM)
# AE <- idparse(AE)
# CSARAW <- idparse(CSARAW)
# EUROQOL <- idparse(EUROQOL)
# KLKNEEOA <- idparse(KLKNEEOA)
# PANAS <- idparse(PANAS)
# SELFE <- idparse(SELFE)
# STUDYSTATUS <- idparse(STUDYSTATUS)
# ACLS <- idparse(ACLS)
# AGE <- idparse(AGE)
# CSQ <- idparse(CSQ)
# EXERCISE <- idparse(EXERCISE)
# KNEESCREENING <- idparse(KNEESCREENING)
# PEDOMETER <- idparse(PEDOMETER)
# SELFMANSESSQA <- idparse(SELFMANSESSQA)
# VAS2 <- idparse(VAS2)
# AHI <- idparse(AHI)
# DEMO <- idparse(DEMO)
# EXRECAP <- idparse(EXRECAP)
# MOS <- idparse(MOS)
# PERSON <- idparse(PERSON)
# SELFMANSUMMARY <- idparse(SELFMANSUMMARY)
# WOMAC <- idparse(WOMAC)
# ADMIN <- idparse(ADMIN)
# CESD <- idparse(CESD)
# ECON <- idparse(ECON)
# FUNCTTEST <- idparse(FUNCTTEST)
# NPI <- idparse(NPI)
# PTASSESSMENT <- idparse(PTASSESSMENT)
# SF36 <- idparse(SF36)

#################################################################
############# BEGIN DATA MANAGEMENT AND HANDLING ################
#################################################################



#######################################################
#######         SCORE INSTRUMENTS               #######
#######################################################


#############
### PANAS ###
#############

# Positive Affect PANASPOS        1,3,5,9,10,12,14,16,17,19
# Negative Affect PANASNEG        2,4,6,7,8,11,13,15,18,20

#library(mice)

PANASpos <- PANAS[,c(1,2,4,6,10,11,13,15,17,18,20)]
PANASneg <- PANAS[,c(1,3,5,7,9,12,14,16,19,21)]

PANASpos.mice <- mice(PANASpos[,-1])
PANASneg.mice <- mice(PANASneg[,-1])

PANASpos.1 <- mean(rowSums(complete(PANASpos.mice,1)))
PANASpos.2 <- mean(rowSums(complete(PANASpos.mice,2)))
PANASpos.3 <- mean(rowSums(complete(PANASpos.mice,3)))
PANASpos.4 <- mean(rowSums(complete(PANASpos.mice,4)))
PANASpos.5 <- mean(rowSums(complete(PANASpos.mice,5)))

PANASneg.1 <- mean(rowSums(complete(PANASneg.mice,1)))
PANASneg.2 <- mean(rowSums(complete(PANASneg.mice,2)))
PANASneg.3 <- mean(rowSums(complete(PANASneg.mice,3)))
PANASneg.4 <- mean(rowSums(complete(PANASneg.mice,4)))
PANASneg.5 <- mean(rowSums(complete(PANASneg.mice,5)))

PANASpos.out <- c(PANASpos.1,PANASpos.2,PANASpos.3,PANASpos.4,PANASpos.5)
PANASneg.out <- c(PANASneg.1,PANASneg.2,PANASneg.3,PANASneg.4,PANASneg.5)

mean(PANASpos.out)
sd(PANASpos.out)

mean(PANASneg.out)
sd(PANASneg.out)

PANAS$positive <- rowSums(complete(PANASpos.mice,1))
PANAS$negative <- rowSums(complete(PANASneg.mice,1))

PANASf <- PANAS[,c(1,22,23)]

##########################
### MOS Social Support ###
##########################

MOS.mice <- mice(MOS[,-c(1,2)])

MOSx.1 <- mean(rowSums(complete(MOS.mice,1)))
MOSx.2 <- mean(rowSums(complete(MOS.mice,2)))
MOSx.3 <- mean(rowSums(complete(MOS.mice,3)))
MOSx.4 <- mean(rowSums(complete(MOS.mice,4)))
MOSx.5 <- mean(rowSums(complete(MOS.mice,5)))

MOSx.out <- c(MOSx.1,MOSx.2,MOSx.3,MOSx.4,MOSx.5)
mean(MOSx.out)
sd(MOSx.out)

MOS$total <- rowMeans(complete(MOS.mice,1))
MOSf <- MOS[,c(1,7)]

#############
### WOMAC ###
#############

WOMACp.mice <- mice(WOMAC[,c(2,3,4,5,6)])
WOMACs.mice <- mice(WOMAC[,c(24,25)])
WOMACd.mice <- mice(WOMAC[,c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)])

psyc.mice(WOMACp.mice)
psyc.mice(WOMACs.mice)
psyc.mice(WOMACd.mice)


WOMAC$pain <- rowMeans(complete(WOMACp.mice,1))
WOMAC$stiffness <- rowMeans(complete(WOMACs.mice,1))
WOMAC$disability <- rowMeans(complete(WOMACd.mice,1))
WOMACf <- WOMAC[,c(1,26:28)]

######################
#### CSQ scoring #####
######################

CSQ.DA.mice <- mice(CSQ[,c(4,10,13,27,28,39)])
CSQ.RPS.mice <- mice(CSQ[,c(2,5,11,17,30,42)])
CSQ.CSS.mice <- mice(CSQ[,c(7,9,21,24,32,33)])
CSQ.IPS.mice <- mice(CSQ[,c(18,20,22,25,31,36)])
CSQ.PH.mice <- mice(CSQ[,c(15,16,19,23,29,37)])
CSQ.CT.mice <- mice(CSQ[,c(6,12,14,26,34,38)])
CSQ.IAL.mice <- mice(CSQ[,c(3,8,35,40,41,43)])

psyc.mice(CSQ.DA.mice)
psyc.mice(CSQ.RPS.mice)
psyc.mice(CSQ.CSS.mice)
psyc.mice(CSQ.IPS.mice)
psyc.mice(CSQ.PH.mice)
psyc.mice(CSQ.CT.mice)
psyc.mice(CSQ.IAL.mice)


# Diverting attention
CSQ$DA <- rowSums(complete(CSQ.DA.mice,1))
# Reinterpreting pain sensations
CSQ$RPS <- rowSums(complete(CSQ.RPS.mice,1))
# Coping self-statements
CSQ$CSS <- rowSums(complete(CSQ.CSS.mice,1))
# Ignoring pain sensations
CSQ$IPS <- rowSums(complete(CSQ.IPS.mice,1))
# Praying or hoping
CSQ$PH <- rowSums(complete(CSQ.PH.mice,1))
# Catastrophizing
CSQ$CT <- rowSums(complete(CSQ.CT.mice,1))
# Increasing activity level
CSQ$IAL  <- rowSums(complete(CSQ.IAL.mice,1))

CSQf <- CSQ[,c(1,46:52)]






######################
#### AHI scoring #####
######################

## The AHI (Arthritis Helplessness Index) consists of 15, 4-point
## Likert scale items.  Reverse score the following items
## 2,3,5,6,8,9,11,13,15 and take the sum total.  Scores should range
## between 15 and 60 with higher scores indicating greater control

#library(car)

recodeAHI <- function(x){
  ritems <- c(2,3,5,6,8,9,11,13,15)
  for (i in 1:length(ritems)){
    x[,ritems[i]+1] <- recode(x[,ritems[i] + 1],"1=4;2=3;3=2;4=1",F)
  }
  return(x)
}

AHIr <- recodeAHI(AHI)

AHIr.mice <- mice(AHIr[,2:15])
psyc.mice(AHIr.mice)

AHIr$AHItotal <- rowSums(complete(AHIr.mice,1))

AHIf <- AHIr[,c(1,17)]

######################
### SF-36 scoring ####
######################

## Shelley sent me the scored version of the SF-36 so I would not have
## to replicate the weighting procedure that was already in place in
## the Access Database.  There are some questions concerning the
## validity of the scoring algorithm but for now we will use the same
## method that was originally setup in the database.

# object for scored SF-36 is SF36s
# object for raw SF-36 is SF36r

# Below is the raw scale scoring method I got from a dialysis scoring
# algorithm from the internet.

SF36r.mice <- mice(SF36r[,-1])

SF36r <- data.frame(ASSESSID=SF36r[,1],complete(SF36r.mice,1))


SF36r$PhysicalFunc <- ((rowSums(SF36r[,4:13],na.rm=T)-10)/20)*100

SF36r$RolePhysical <- ((rowSums(SF36r[,14:17],na.rm=T)-4)/4)*100

BodyPainV1 <- recode(SF36r[,22],"1=6.0;2=5.4;3=4.2;4=3.1;5=2.2;6=1",F)
BodyPainV2 <- rep(NA,nrow(SF36r))
BodyPainV2[SF36r[,22]==1 & SF36r[,23]==1] <- 6.0
BodyPainV2[SF36r[,22]!=1 & SF36r[,23]==1] <- 5.0
BodyPainV2[SF36r[,23]==2] <- 4.0
BodyPainV2[SF36r[,23]==3] <- 3.0
BodyPainV2[SF36r[,23]==4] <- 2.0
BodyPainV2[SF36r[,23]==5] <- 1.0
SF36r$BodyPain <- (((BodyPainV1 + BodyPainV2)-2)/10)*100

GHrV1 <- recode(SF36r[,2],"1=5.0;2=4.4;3=3.4;4=2.0;5=1.0",F)
GHrV2 <- recode(SF36r[,35],"1=5.0;2=4.0;3=3.0;4=2.0;5=1.0",F)
GHrV3 <- recode(SF36r[,37],"1=5.0;2=4.0;3=3.0;4=2.0;5=1.0",F)
SF36r$GeneralHealth <- (((GHrV1 + SF36r[,34] + SF36r[,36] + GHrV3)-5)/20)*100

VrV1 <-  recode(SF36r[,24],"1=6.0;2=5.0;3=4.0;4=3.0;5=2.0;6=1.0",F)
VrV2 <-  recode(SF36r[,28],"1=6.0;2=5.0;3=4.0;4=3.0;5=2.0;6=1.0",F)
SF36r$Vitality <- (((VrV1 + VrV2 + SF36r[,30] + SF36r[,32])-4)/20)*100

SFrV1 <-  recode(SF36r[,21],"1=5.0;2=4.0;3=3.0;4=2.0;5=1.0",F)
SF36r$SocialFunc <- (((SFrV1 + SF36r[,33])-2)/8)*100

SF36r$RoleEmotional <- ((rowSums(SF36r[,18:20]) - 3)/3)*100

MHrV1 <-  recode(SF36r[,27],"1=6.0;2=5.0;3=4.0;4=3.0;5=2.0;6=1.0",F)
MHrV2 <-  recode(SF36r[,31],"1=6.0;2=5.0;3=4.0;4=3.0;5=2.0;6=1.0",F)
SF36r$MentalHealth <- (((MHrV1 + MHrV2 + SF36r[,25] + SF36r[,26] + SF36r[,29])-5)/25)*100

SF36r$PhysicalHealth <- rowMeans(SF36r[,38:42],na.rm=T)
SF36r$MentalHealth <- rowMeans(SF36r[,41:45],na.rm=T)
SF36r$SF36Total <- rowMeans(SF36r[,38:45],na.rm=T)

SF36rf <- SF36r[,c(1,38:47)]
## SF36sf <- SF36s[,c(1,3,4,5,6,7,8,10,9,11,2)]

## #check scoring for PhysicalFunction

## SF36rFF <- SF36r[,c(1,38)]
## SF36sFF <- SF36s[,c(1,3)]

## SF36test <- merge(SF36rFF,SF36sFF,by="ASSESSID")

##################################
### NEOPI Extraversion Scoring ###
##################################

### NB: Code copied from the ALMA code but since ASSESSID is included,
### the variable locations had to be incremented by 1 below.  

# recode items

NPIr <- NPI
ritems.NPI <- c(8,15,21,25,27,28,35,37)

for (i in 1:length(ritems.NPI)){
  NPIr[,ritems.NPI[i] + 1] <- abs(NPIr[,ritems.NPI[i]+1] - 6)
}

NPIr$warm <- rowMeans(NPIr[,c(33,38,30,36,3,35,27,31)],na.rm=T)
NPIr$greg <- rowMeans(NPIr[,c(29,15,9,37,39,34,28)],na.rm=T)
NPIr$assr <- NPIr[,26]
NPIr$acti <- rowMeans(NPIr[,c(8,25,21)],na.rm=T)
NPIr$exci <- NPIr[,18]
NPIr$emot <- rowMeans(NPIr[,c(22,16,5,11)],na.rm=T)

## NPIr.mice <- mice(NPIr[,c(39:44)]) # no missing values found

# total Extraversion score
NPIr$extra <- rowMeans(NPIr[,c(39:44)],na.rm=T)
NPIrf <- NPIr[,c(1,45)]

#############
### CES-D ###
#############

## Items to be reverse scored:  4,8,12,16 

CESDr <- CESD
CESDr[,5] <- recode(CESDr[,5],"0=3;1=2;2=1;3=0",F)
CESDr[,9] <- recode(CESDr[,9],"0=3;1=2;2=1;3=0",F)
CESDr[,13] <- recode(CESDr[,13],"0=3;1=2;2=1;3=0",F)
CESDr[,17] <- recode(CESDr[,17],"0=3;1=2;2=1;3=0",F)

#library(mice)

CESDr.mice <- mice(CESDr[,-1])
CESDr.efa1 <- factanal(complete(CESDr.mice,1),5)

totx.1 <- mean(rowSums(complete(CESDr.mice,1)))
totx.2 <- mean(rowSums(complete(CESDr.mice,2)))
totx.3 <- mean(rowSums(complete(CESDr.mice,3)))
totx.4 <- mean(rowSums(complete(CESDr.mice,4)))
totx.5 <- mean(rowSums(complete(CESDr.mice,5)))

CESDmeans <- c(totx.1,totx.2,totx.3,totx.4,totx.5)

mean(CESDmeans)
sd(CESDmeans)

CESDr$CESDtotal <- rowSums(complete(CESDr.mice,1))

CESDrf <- CESDr[,c(1,22)]

# missing data not a problem for the CESD data so use a single imputation for complete cases


###############################
### Arthritis Self-Efficacy ###
###############################

SEp.mice <- mice(SELFE[,2:6])
SEf.mice <- mice(SELFE[,7:15])
SEo.mice <- mice(SELFE[,16:21])

psyc.mice(SEp.mice)
psyc.mice(SEf.mice)
psyc.mice(SEo.mice)


SELFE$Pain <- rowMeans(complete(SEp.mice,1))
SELFE$Function <- rowMeans(complete(SEf.mice,1))
SELFE$OtherSx <- rowMeans(complete(SEo.mice,1))
SELFEf <- SELFE[,c(1,22:24)]

## prepare data for Julius to analyze

julius <- idparse(SELFE)

julius$month <- NA
julius$month[julius$assessid==501] <- 0
julius$month[julius$assessid==520] <- 0
julius$month[julius$assessid==521] <- 3
julius$month[julius$assessid==540] <- 9
julius$month[julius$assessid==522] <- 18
julius$month[julius$assessid==541] <- 24

julius.dat <- julius[,c(26,28,2:21)]

write.csv(julius.dat,"./FinalData/SEELFjulius.csv",row.names=FALSE)






########################
### Euroquol (EQ-5D) ###
########################

EUROQOL.mice <- mice(EUROQOL[,-1])

psyc.mice(EUROQOL.mice)

EUROQOL <- data.frame(ASSESSID=EUROQOL$ASSESSID,complete(EUROQOL.mice,1))

m1 <- rep(0,nrow(EUROQOL))
m2 <- m1
s1 <- m1
s2 <- m1
u1 <- m1
u2 <- m1
p1 <- m1
p2 <- m1
a1 <- m1
a2 <- m1

m1[EUROQOL[,2]==2] <- 1
m2[EUROQOL[,2]==3] <- 1
s1[EUROQOL[,3]==2] <- 1
s2[EUROQOL[,3]==3] <- 1
u1[EUROQOL[,4]==2] <- 1
u2[EUROQOL[,4]==3] <- 1
p1[EUROQOL[,5]==2] <- 1
p2[EUROQOL[,5]==3] <- 1
a1[EUROQOL[,6]==2] <- 1
a2[EUROQOL[,6]==3] <- 1

m0 <- rep(0,nrow(EUROQOL))
s0 <- m0
u0 <- m0
p0 <- m0
a0 <- m0

m0[m1 == 0 & m2 == 0] <- 1
s0[s1 == 0 & s2 == 0] <- 1
u0[u1 == 0 & u2 == 0] <- 1
p0[p1 == 0 & p2 == 0] <- 1
a0[a1 == 0 & a2 == 0] <- 1

i2 <- m1 + s1 + u1 + p1 + a1
i2 <- i2 - 1
i2[i2 < 0] <- 0
i22 <- i2*i2

i3 <- m2 + s2 + u2 + p2 + a2
i3 <- i3 - 1
i3[i3 < 0] <- 0
i32 <- i3*i3

i1 <- m0 + s0 + u0 + p0 + a0
d1 <- 4 - i1
d1[d1 < 0] <- 0

EUROQOL$pred <- .146016*m1 + .557685*m2 + .1753425*s1 + .4711896*s2 + .1397295*u1 + .3742594*u2 + .1728907*p1 + .5371011*p2 + .156223*a1 + .4501876*a2 + -.1395949*d1 + .0106868*i22  + -.1215579*i3 + -.0147963*i32 

EUROQOL$EQindex <- 1 - EUROQOL$pred
EUROQOL$EQindex[is.na(EUROQOL[,2]) | is.na(EUROQOL[,3]) | is.na(EUROQOL[,4]) | is.na(EUROQOL[,5]) | is.na(EUROQOL[,6])] <- NA

EQ5Df <- EUROQOL[,c(1,8)]


##################################################
### Functional outcomes
#########

## for MTM (methods time measurement) average performance
## 70-80% on ERGOS entry level for workforce competitiveness
## 80-100% is competitive for workforce
##




#####################################################
#####          END INSTRUMENT SCORING          ######
#####################################################


###########################################
#######  Combine Data for Analysis ########
###########################################

## I created a key for the study pemKEY.csv:

## "PROGRAMID","SUBGROUPID","COHORT","GROUP","GLabel"
## 5,0,1,0,"Pre-randomized"
## 5,1,1,1,"Exercise"
## 5,2,1,2,"Self-Management"
## 5,3,1,3,"Combined"
## 5,4,2,0,"Pre-randomized"
## 5,5,2,1,"Exercise"
## 5,6,2,2,"Self-Management"
## 5,7,2,3,"Combined"
## 5,8,3,0,"Pre-randomized"
## 5,9,3,1,"Exercise"
## 5,10,3,2,"Self-Management"
## 5,11,3,3,"Combined"
## 5,12,4,0,"Pre-randomized",,
## 5,13,4,1,"Exercise"
## 5,14,4,2,"Self-Management"
## 5,15,4,3,"Combined"
## 5,16,5,0,"Pre-randomized"
## 5,17,5,1,"Exercise"
## 5,18,5,2,"Self-Management"
## 5,19,5,3,"Combined"

pemKEY <- read.csv("./FinalData/pemKEY.csv",header=T)

###############################################################################
# Function to parse the assessid variable to recover subject id and time vars #
###############################################################################

idparse <- function(x){
  x$study <- as.integer(substr(as.character(x$ASSESSID),1,2))
  x$personid <- as.integer(substr(as.character(x$ASSESSID),3,6))
  x$assessid <- as.integer(substr(as.character(x$ASSESSID),7,9))
  return(x)
}

###########################################
## Function to recode assessid to month ###
###########################################

assessidparse <- function(x){
  x$month <- recode(x$assessid,"501=0;520=0;521=3;540=9;522=18;541=24;530=1;531=2;532=6;533=12;534=15;535=21",F)
  return(x)
}

PANASff <- assessidparse(idparse(PANASf))[,c("personid","month","positive","negative")]
MOSff <- assessidparse(idparse(MOSf))[,c("personid","month","total")]
names(MOSff)[3] <- c("MOStotal")
WOMACff <- assessidparse(idparse(WOMACf))[,c("personid","month","pain","stiffness","disability")]
CSQff <- assessidparse(idparse(CSQf))[,c("personid","month","DA","RPS","CSS","IPS","PH","CT","IAL")]
SF36rff <- assessidparse(idparse(SF36rf))[,c(13,15,2:11)]
SELFEff <- assessidparse(idparse(SELFEf))[,c("personid","month","Pain","Function","OtherSx")]
VAS2f <- assessidparse(idparse(VAS2))[,c("personid","month","KNEEPAINVAS","ARTHRITISVAS")]
EQ5Dff <- assessidparse(idparse(EQ5Df))[,c("personid","month","EQindex")]
CESDrff <- assessidparse(idparse(CESDrf))[,c("personid","month","CESDtotal")]

KNEEf <- merge(PANASff,MOSff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,WOMACff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,CSQff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,SF36rff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,SELFEff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,VAS2f,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,EQ5Dff,all=T,by=c("personid","month"))
KNEEf <- merge(KNEEf,CESDrff,all=T,by=c("personid","month"))

# replace SF36sf with rf if curious about the scoring

KNEEf <- merge(KNEEf,PERSON[,c(1,2,3,4)],by.x="personid",by.y="PERSONID")
KNEEf <- merge(KNEEf,pemKEY,all=T,by="SUBGROUPID")


## CESD.final <- idparse(CESDrf)
## CESD.final$month <- NA
## CESD.final$month[CESD.final$assessid==501] <- 0
## CESD.final$month[CESD.final$assessid==520] <- 0
## CESD.final$month[CESD.final$assessid==521] <- 3
## CESD.final$month[CESD.final$assessid==540] <- 9
## CESD.final$month[CESD.final$assessid==522] <- 18
## CESD.final$month[CESD.final$assessid==541] <- 24

write.csv(KNEEf,"./FinalData/KNEEnotreduced.csv",row.names=FALSE)

# Measures that are not necessarily time dependent or have different time stamps that cannot be merged with the outcomes 1 and outcomes 2 measures

KNEEstatic <- merge(AHIf,NPIrf,all=T,by="ASSESSID")
## KNEEstatic <- merge(KNEEstatic,CESDrf,all=T,by="ASSESSID") ## CES-D has 3 time points now so I moved it up with the time varying measures
KNEEstatic <- idparse(KNEEstatic)
KNEEstatic <- merge(KNEEstatic,PERSON[,c(1,2,3,4)],by.x="personid",by.y="PERSONID",all=T)
KNEEstatic <- KNEEstatic[!is.na(KNEEstatic$personid),]

write.csv(KNEEstatic,"./FinalData/KNEEstatic.csv",row.names=FALSE)

## AHIf
## NPIrf

RelCheck <- function(x){
  dat <- idparse(x)
  dat$month <- NA
  dat$month[dat$assessid==501] <- 0
  dat$month[dat$assessid==520] <- 0
  dat$month[dat$assessid==521] <- 3
  dat$month[dat$assessid==540] <- 9
  dat$month[dat$assessid==522] <- 18
  dat$month[dat$assessid==541] <- 24
  
  idat <- dat[,-c(1,(ncol(dat)-1),(ncol(dat)-2),(ncol(dat)-3))]

  mean.alpha <- mean(by(idat[,-ncol(idat)],idat$month,function(x) c.alpha(x)[[5]]))

  dat$scores <- rowSums(idat[,-ncol(idat)])
  months <- sort(unique(dat$month))
  rxx <- rep(NA,(length(months)-1))
  sdat <- data.frame(id=dat$personid,month=dat$month,scores=dat$scores)
  
  for (i in 1:(length(months)-1)){
    tmp1 <- sdat[sdat$month==months[i],]
    tmp2 <- sdat[sdat$month==months[i+1],]
    s.dat <- merge(tmp1,tmp2,all=T,"id")
    rxx[i] <- cor(s.dat[,3],s.dat[,5],use="pairwise.complete.obs")
  }

  mean.rxx <- mean(rxx)

  res <- round(c(mean.alpha,mean.rxx),2)
  
  return(res)
}

### below are the mean alpha's and mean test-retest reliabilities for the various measures

# PANAS positive
RelCheck(PANASpos)
#[1] 0.92 0.70

# PANAS negative
RelCheck(PANASneg)
#[1] 0.87 0.63

# CT
RelCheck(CSQ[,c(1,6,12,14,26,34,38)])
#[1] 0.79 0.49

# SE.p
RelCheck(SELFE[,c(1,2:6)])
#[1] 0.85 0.48

# SE.f
RelCheck(SELFE[,c(1,7:15)])
#[1] 0.89 0.46

# SE.o
RelCheck(SELFE[,c(1,16:21)])
#[1] 0.90 0.62

# SE.all
RelCheck(SELFE[,c(1,2:21)])
#[1] 0.92 0.57

# Physical Functioning SF-36
RelCheck(SF36r[,c(1,4:13)])
#[1] 0.85 0.61


## NOTE: the lower values for test-retest are actually good because it
## means that the scores over time are not just reflecting the
## baseline values observed prior to treatment.  We should be happy
## that some of the test-retest values are low.

source("/home/pem/files/PAT/Research/Code-current/MRES/MRES.R")

panasp.igc <- igc(KNEEf,"personid","month","positive")
panasn.igc <- igc(KNEEf,"personid","month","negative")
csqDA.igc <- igc(KNEEf,"personid","month","DA")
csqRPS.igc <- igc(KNEEf,"personid","month","RPS")
csqCSS.igc <- igc(KNEEf,"personid","month","CSS")
csqIPS.igc <- igc(KNEEf,"personid","month","IPS")
csqPH.igc <- igc(KNEEf,"personid","month","PH")
csqCT.igc <- igc(KNEEf,"personid","month","CT")
csqIAL.igc <- igc(KNEEf,"personid","month","IAL")
sf36tot.igc <- igc(KNEEf,"personid","month","SF36Total")
sf36pf.igc <- igc(KNEEf,"personid","month","PhysicalFunc")
sf36bp.igc <- igc(KNEEf,"personid","month","BodyPain")
sf36ph.igc <- igc(KNEEf,"personid","month","PhysicalHealth")
sepain.igc <- igc(KNEEf,"personid","month","Pain")
sefcn.igc <- igc(KNEEf,"personid","month","Function")
seosx.igc <- igc(KNEEf,"personid","month","OtherSx")
kpainvas.igc <- igc(KNEEf,"personid","month","KNEEPAINVAS")
arthvas.igc <- igc(KNEEf,"personid","month","ARTHRITISVAS")
eq5d.igc <- igc(KNEEf,"personid","month","EQindex")

panasp.igcML <- igc(KNEEf,"personid","month","positive",method="ML")
panasn.igcML <- igc(KNEEf,"personid","month","negative",method="ML")
csqDA.igcML <- igc(KNEEf,"personid","month","DA",method="ML")
csqRPS.igcML <- igc(KNEEf,"personid","month","RPS",method="ML")
csqCSS.igcML <- igc(KNEEf,"personid","month","CSS",method="ML")
csqIPS.igcML <- igc(KNEEf,"personid","month","IPS",method="ML")
csqPH.igcML <- igc(KNEEf,"personid","month","PH",method="ML")
csqCT.igcML <- igc(KNEEf,"personid","month","CT",method="ML")
csqIAL.igcML <- igc(KNEEf,"personid","month","IAL",method="ML")
sf36tot.igcML <- igc(KNEEf,"personid","month","SF36Total",method="ML")
sf36pf.igcML <- igc(KNEEf,"personid","month","PhysicalFunc",method="ML")
sf36bp.igcML <- igc(KNEEf,"personid","month","BodyPain",method="ML")
sf36ph.igcML <- igc(KNEEf,"personid","month","PhysicalHealth",method="ML")
sepain.igcML <- igc(KNEEf,"personid","month","Pain",method="ML")
sefcn.igcML <- igc(KNEEf,"personid","month","Function",method="ML")
seosx.igcML <- igc(KNEEf,"personid","month","OtherSx",method="ML")
kpainvas.igcML <- igc(KNEEf,"personid","month","KNEEPAINVAS",method="ML")
arthvas.igcML <- igc(KNEEf,"personid","month","ARTHRITISVAS",method="ML")
eq5d.igcML <- igc(KNEEf,"personid","month","EQindex",method="ML")

## unconditional plots

pdf("./FinalData/UnconditionedGrowthCurvePlots.pdf")
plot(panasp.igc,xlab="Month",ylab="PANAS Positive",main="PANAS Positive IGC")
plot(panasn.igc,xlab="Month",ylab="PANAS Negative",main="PANAS Negative IGC")
plot(csqDA.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Diverting Attention")
plot(csqRPS.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Reinterpreting Pain Sensations")
plot(csqCSS.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Coping Self-Statements")
plot(csqIPS.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Ignoring Pain Sensations")
plot(csqPH.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Praying or Hoping")
plot(csqCT.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Catastrophizing")
plot(csqIAL.igc,xlab="Month",ylab="CSQ Score",main="CSQ - Increasing Activity Level")
plot(sf36tot.igc,xlab="Month",ylab="SF36 Score",main="SF36 - Total Score")
plot(sf36pf.igc,xlab="Month",ylab="SF36 Score",main="SF36 - Physical Functioning")
plot(sf36bp.igc,xlab="Month",ylab="SF36 Score",main="SF36 - Body Pain")
plot(sf36ph.igc,xlab="Month",ylab="SF36 Score",main="SF36 - Physical Health")
plot(sepain.igc,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Pain")
plot(sefcn.igc,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Functioning")
plot(seosx.igc,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Other Symtoms")
plot(kpainvas.igc,xlab="Month",ylab="VAS Score",main="Knee Pain VAS")
plot(arthvas.igc,xlab="Month",ylab="VAS Score",main="Arthritis VAS")
plot(eq5d.igc,xlab="Month",ylab="EQ-5D Score",main="EQ-5D Total Score")
dev.off()


## unconditional ML plots

pdf("./FinalData/UnconditionedMLGrowthCurvePlots.pdf")
plot(panasp.igcML,xlab="Month",ylab="PANAS Positive",main="PANAS Positive IGC")
plot(panasn.igcML,xlab="Month",ylab="PANAS Negative",main="PANAS Negative IGC")
plot(csqDA.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Diverting Attention")
plot(csqRPS.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Reinterpreting Pain Sensations")
plot(csqCSS.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Coping Self-Statements")
plot(csqIPS.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Ignoring Pain Sensations")
plot(csqPH.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Praying or Hoping")
plot(csqCT.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Catastrophizing")
plot(csqIAL.igcML,xlab="Month",ylab="CSQ Score",main="CSQ - Increasing Activity Level")
plot(sf36tot.igcML,xlab="Month",ylab="SF36 Score",main="SF36 - Total Score")
plot(sf36pf.igcML,xlab="Month",ylab="SF36 Score",main="SF36 - Physical Functioning")
plot(sf36bp.igcML,xlab="Month",ylab="SF36 Score",main="SF36 - Body Pain")
plot(sf36ph.igcML,xlab="Month",ylab="SF36 Score",main="SF36 - Physical Health")
plot(sepain.igcML,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Pain")
plot(sefcn.igcML,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Functioning")
plot(seosx.igcML,xlab="Month",ylab="Self-Efficacy Score",main="SELFE - Other Symtoms")
plot(kpainvas.igcML,xlab="Month",ylab="VAS Score",main="Knee Pain VAS")
plot(arthvas.igcML,xlab="Month",ylab="VAS Score",main="Arthritis VAS")
plot(eq5d.igcML,xlab="Month",ylab="EQ-5D Score",main="EQ-5D Total Score")
dev.off()

## conditioned plots


###########################################
## Data Reduction

extractigc <- function(x,prefix){
  dat <- x$params
  names(dat) <- c("personid",paste(prefix,"I",sep=""),paste(prefix,"L",sep=""),paste(prefix,"Ise",sep=""),paste(prefix,"Lse",sep=""),paste(prefix,"Rsq",sep=""))
  return(dat)
}

panasp <- extractigc(panasp.igc,"panasp")
panasn <- extractigc(panasn.igc,"panasn")
csqDA <- extractigc(csqDA.igc,"csqDA")
csqRPS <- extractigc(csqRPS.igc,"csqRPS")
csqCSS <- extractigc(csqCSS.igc,"csqCSS")
csqIPS <- extractigc(csqIPS.igc,"csqIPS")
csqPH <- extractigc(csqPH.igc,"csqPH")
csqCT <- extractigc(csqCT.igc,"csqCT")
csqIAL <- extractigc(csqIAL.igc,"csqIAL")
sf36tot <- extractigc(sf36tot.igc,"sf36tot")
sf36pf <- extractigc(sf36pf.igc,"sf36pf")
sf36bp <- extractigc(sf36bp.igc,"sf36bp")
sf36ph <- extractigc(sf36ph.igc,"sf36ph")
sepain <- extractigc(sepain.igc,"sepain")
sefcn <- extractigc(sefcn.igc,"sefcn")
seosx <- extractigc(seosx.igc,"seosx")
kpainvas <- extractigc(kpainvas.igc,"kpainvas")
arthvas <- extractigc(arthvas.igc,"arthvas")
eq5d <-  extractigc(eq5d.igc,"eq5d")

## merge growth curve data

all.igcs <- merge(panasp,panasn,by="personid",all=T)
all.igcs <- merge(all.igcs,csqDA,by="personid",all=T)
all.igcs <- merge(all.igcs,csqRPS,by="personid",all=T)
all.igcs <- merge(all.igcs,csqCSS,by="personid",all=T)
all.igcs <- merge(all.igcs,csqIPS,by="personid",all=T)
all.igcs <- merge(all.igcs,csqPH,by="personid",all=T)
all.igcs <- merge(all.igcs,csqCT,by="personid",all=T)
all.igcs <- merge(all.igcs,csqIAL,by="personid",all=T)
all.igcs <- merge(all.igcs,sf36tot,by="personid",all=T)
all.igcs <- merge(all.igcs,sf36pf,by="personid",all=T)
all.igcs <- merge(all.igcs,sf36bp,by="personid",all=T)
all.igcs <- merge(all.igcs,sf36ph,by="personid",all=T)
all.igcs <- merge(all.igcs,sepain,by="personid",all=T)
all.igcs <- merge(all.igcs,sefcn,by="personid",all=T)
all.igcs <- merge(all.igcs,seosx,by="personid",all=T)
all.igcs <- merge(all.igcs,kpainvas,by="personid",all=T)
all.igcs <- merge(all.igcs,arthvas,by="personid",all=T)
all.igcs <- merge(all.igcs,eq5d,by="personid",all=T)

all.igcs <- merge(all.igcs,PERSON[,c(1,2,3,4)],by.x="personid",by.y="PERSONID")
all.igcs <- merge(all.igcs,pemKEY,all=T,by="SUBGROUPID")

all.igcs <- all.igcs[!is.na(all.igcs$personid),]

## residualize out intercepts from slopes before aggregating

all.igcs$panaspRes <- resid(lm(all.igcs[,4]~all.igcs[,3],na.action=na.exclude))
all.igcs$panasnRes <- resid(lm(all.igcs[,9]~all.igcs[,8],na.action=na.exclude))
all.igcs$csqDARes <- resid(lm(all.igcs[,14]~all.igcs[,13],na.action=na.exclude))
all.igcs$csqRPSRes <- resid(lm(all.igcs[,19]~all.igcs[,18],na.action=na.exclude))
all.igcs$csqCSSRes <- resid(lm(all.igcs[,24]~all.igcs[,23],na.action=na.exclude))
all.igcs$csqIPSRes <- resid(lm(all.igcs[,29]~all.igcs[,28],na.action=na.exclude))
all.igcs$csqPHRes <- resid(lm(all.igcs[,34]~all.igcs[,33],na.action=na.exclude))
all.igcs$csqCTRes <- resid(lm(all.igcs[,39]~all.igcs[,38],na.action=na.exclude))
all.igcs$csqIALRes <- resid(lm(all.igcs[,44]~all.igcs[,43],na.action=na.exclude))
all.igcs$sf36totRes <- resid(lm(all.igcs[,49]~all.igcs[,48],na.action=na.exclude))
all.igcs$sf36pfRes <- resid(lm(all.igcs[,54]~all.igcs[,53],na.action=na.exclude))
all.igcs$sf36bpRes <- resid(lm(all.igcs[,59]~all.igcs[,58],na.action=na.exclude))
all.igcs$sf36phRes <- resid(lm(all.igcs[,64]~all.igcs[,63],na.action=na.exclude))
all.igcs$sepainRes <- resid(lm(all.igcs[,69]~all.igcs[,68],na.action=na.exclude))
all.igcs$sefcnRes <- resid(lm(all.igcs[,74]~all.igcs[,73],na.action=na.exclude))
all.igcs$seosxRes <- resid(lm(all.igcs[,79]~all.igcs[,78],na.action=na.exclude))
all.igcs$kpainvasRes <- resid(lm(all.igcs[,84]~all.igcs[,83],na.action=na.exclude))
all.igcs$arthvasRes <- resid(lm(all.igcs[,89]~all.igcs[,88],na.action=na.exclude))
all.igcs$eq5dRes <- resid(lm(all.igcs[,94]~all.igcs[,93],na.action=na.exclude))


knee <- merge(all.igcs,KNEEstatic[,c(1,3,4)],all.y=T,by="personid")

write.csv(knee[,-100],"./FinalData/KNEEreduced.csv",row.names=FALSE)

#################################################################
############# END DATA MANAGEMENT AND HANDLING ##################
#################################################################


##########################################################################
####### SKIP STEP ABOVE AND USE THE FOLLOWING CODE TO READ IN DATA #######
##########################################################################

## reduced data
knee <- read.csv("./FinalData/KNEEreduced.csv")


## non-reduced data
KNEEf <- read.csv("./FinalData/KNEEnotreduced.csv")
KNEEstatic <- read.csv("./FinalData/KNEEstatic.csv")


#################################################################
############ MISSING DATA HANDLING PROCEDURES ###################
#################################################################

source("/home/pem/files/PAT/Research/Code-current/MRES/MRES.R")

# knee.md <- mddiag(knee)

missplot <- function(x){
  dat <- as.matrix(x)
  Dmat <- as.matrix(1 * is.na(dat))
  mdp <- Dmat %*% (2^(1:ncol(Dmat)))
  couple <- data.frame(Dmat,mdp) # combine Dmat and mdp
  nc <- couple[rev(order(mdp)),]
  dmat2 <- as.matrix(nc)
  killlast <- dmat2[,(-1 * ncol(dmat2))]
  nd <- t(killlast)
  xdim <- 1:nrow(Dmat)
  ydim <- 1:ncol(Dmat)
  image(z=nd,col=c("black","white"),axes=F,xlab="Variables",ylab="Observations")
}

missplot(knee)
missplot(knee.l)

## Kill observations that are missing on the identification variables
## (done with the all.y=T in the merge statement at the end of the
## data management and handling section.

## Now we want to kill all observations that have no data from the
## study.

kneeNONA <- knee[!is.na(knee$SUBGROUPID),]

missplot(kneeNONA)

# from this point forward, the knee dataset will be the kneeNONA

knee <- kneeNONA

### treat missing data with mice (use below for individual analyses)

## library(mice)

## knee.mids <- mice(knee[,c(1,3:124)])

## str(knee.mids)

#################################################################
#################### BEGIN ANALYSES #############################
#################################################################

#library(gplots)

knee$GROUP <- as.factor(knee$GROUP)


##########################################
### Extraversion ---> Positive Affect ####
##########################################

## link 1 Extraversion predicts change in positive affect

l1.lm <- lm(panaspRes~extra,knee)
summary(l1.lm)

## Call:
## lm(formula = panaspL ~ extra, data = knee)

## Residuals:
##       Min        1Q    Median        3Q       Max 
## -1.995721 -0.346136 -0.002270  0.320837  3.072031 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)  0.24621    0.43986   0.560    0.576
## extra       -0.05147    0.15250  -0.338    0.736

## Residual standard error: 0.6024 on 199 degrees of freedom
##   (66 observations deleted due to missingness)
## Multiple R-Squared: 0.0005722,	Adjusted R-squared: -0.00445 
## F-statistic: 0.1139 on 1 and 199 DF,  p-value: 0.736 



##################################
### Extraversion ---> Coping  ####
##################################

EX.csqDARes <- lm(csqDARes~extra,knee) # ns
EX.csqDAI <- lm(csqDAI~extra,knee)
summary(EX.csqDAI)

## Call:
## lm(formula = csqDAI ~ extra, data = knee)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -12.0934  -6.1617  -0.5742   4.9458  18.4615 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   20.652      5.218   3.958 0.000105 ***
## extra         -3.595      1.805  -1.991 0.047817 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 7.411 on 200 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-Squared: 0.01944,	Adjusted R-squared: 0.01454 
## F-statistic: 3.965 on 1 and 200 DF,  p-value: 0.04782 

plot(csqDAI~extra,xlab="Extraversion",ylab="Baseline Coping Style: Diverting Attention",data=knee)
abline(EX.csqDAI,col="blue",lwd=2)
dev2bitmap("EXcsqDAI.png")

EX.csqRPSRes <- lm(csqRPSRes~extra,knee) # ns
EX.csqRPSI <- lm(csqRPSI~extra,knee) # ns

EX.csqCSSRes <- lm(csqCSSRes~extra,knee) # ns
EX.csqCSSI <- lm(csqCSSI~extra,knee) # ns

EX.csqIPSRes <- lm(csqIPSRes~extra,knee) # ns
EX.csqIPSI <- lm(csqIPSI~extra,knee) # ns
summary(EX.csqIPSI)

## Call:
## lm(formula = csqIPSI ~ extra, data = knee)

## Residuals:
##       Min        1Q    Median        3Q       Max 
## -16.32227  -6.24412   0.03345   5.98070  16.71164 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)    8.228      5.417   1.519   0.1304  
## extra          3.182      1.875   1.697   0.0913 .
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 7.7 on 200 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-Squared: 0.01419,	Adjusted R-squared: 0.009266 
## F-statistic:  2.88 on 1 and 200 DF,  p-value: 0.09125 

EX.csqPHRes <- lm(csqPHRes~extra,knee) # ns
EX.csqPHI <- lm(csqPHI~extra,knee) # ns
summary(EX.csqPHI)

## Call:
## lm(formula = csqPHI ~ extra, data = knee)

## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.486 -5.564 -2.342  4.469 20.845 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   17.537      5.111   3.431 0.000729 ***
## extra         -3.321      1.771  -1.875 0.062196 .  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 7.258 on 202 degrees of freedom
##   (4 observations deleted due to missingness)
## Multiple R-Squared: 0.01711,	Adjusted R-squared: 0.01225 
## F-statistic: 3.517 on 1 and 202 DF,  p-value: 0.0622 

EX.csqCTRes <- lm(csqCTRes~extra,knee) #ns
EX.csqCTI <- lm(csqCTI~extra,knee) #ns
summary(EX.csqCTI)

## Call:
## lm(formula = csqCTI ~ extra, data = knee)

## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.627 -2.185 -1.218  1.004 12.941 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   6.1792     2.3098   2.675   0.0081 **
## extra        -1.3682     0.8009  -1.708   0.0891 . 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 3.236 on 196 degrees of freedom
##   (10 observations deleted due to missingness)
## Multiple R-Squared: 0.01467,	Adjusted R-squared: 0.009647 
## F-statistic: 2.919 on 1 and 196 DF,  p-value: 0.08913 


EX.csqIALRes <- lm(csqIALRes~extra,knee) # ns
EX.csqIALI <- lm(csqIALI~extra,knee)
summary(EX.csqIALI)

## Call:
## lm(formula = csqIALI ~ extra, data = knee)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.6673  -6.3425  -0.5167   5.7631  20.3753 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   28.009      5.762   4.861 2.36e-06 ***
## extra         -4.866      1.993  -2.441   0.0155 *  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 8.151 on 199 degrees of freedom
##   (7 observations deleted due to missingness)
## Multiple R-Squared: 0.02907,	Adjusted R-squared: 0.0242 
## F-statistic: 5.959 on 1 and 199 DF,  p-value: 0.01552 

plot(csqIALI~extra,xlab="Extraversion",ylab="Baseline Coping Style: Increasing Activity Level",data=knee)
abline(EX.csqIALI,col="blue",lwd=2)
dev2bitmap("EXcsqIALI.png")


################################
### Tx ---> POS Affect (yes) ###
################################

## Affect
panasp.aov <-aov(panaspRes~ GROUP,knee)
panasn.aov <-aov(panasnRes~ GROUP,knee)
## Summary: Tx related to change in affect when intercept is
## partialled out


summary(panasp.aov)

##              Df Sum Sq Mean Sq F value  Pr(>F)  
## GROUP         2  2.425   1.212  3.8808 0.02222 *
## Residuals   198 61.858   0.312                  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
## 7 observations deleted due to missingness


plotmeans(panaspRes~GROUP,data=knee,col="red",connect=F,barwidth=2,legends=c("Ex","SM","EX+SM"),ylab="Change in Positive Affect")

dev2bitmap("panaposGROUP.png")

summary(lm(panasp.aov))

## Call:
## lm(formula = panasp.aov)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.92939 -0.33992  0.02345  0.28897  2.48788 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  0.01363    0.06778   0.201   0.8408  
## GROUP2      -0.16156    0.09621  -1.679   0.0947 .
## GROUP3       0.10644    0.09658   1.102   0.2718  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 0.5589 on 198 degrees of freedom
##   (7 observations deleted due to missingness)
## Multiple R-Squared: 0.03772,	Adjusted R-squared: 0.028 
## F-statistic: 3.881 on 2 and 198 DF,  p-value: 0.02222 

summary(panasn.aov)

##              Df  Sum Sq Mean Sq F value Pr(>F)
## GROUP         2  0.2743  0.1371  0.8505 0.4288
## Residuals   196 31.6077  0.1613               
## 9 observations deleted due to missingness

#####################################
#### Tx ---> Coping (perhaps) #######
#####################################

## Coping
csqDA.aov <- aov(csqDARes~ GROUP*panaspI,knee) # ns
csqRPS.aov <- aov(csqRPSRes~GROUP*panaspRes+GROUP*panaspI,knee) # ns
csqCSS.aov <- aov(csqCSSRes~GROUP*panaspI,knee) # ns
csqIPS.aov <- aov(csqIPSRes~GROUP*panaspRes,knee) # ns
## NOTE: SM lower than EX group with IPS but EX and COMBO no
## different, however, the effect goes away when I include the panas.
## Interesting, though, the panasp interacts with the contrast between
## the COMBO group and the EX group.  The estimate is negative,
## therefore indicating that the higher the positive affect, the less
## change we should expect for the COMBO group compared to the EX
## group for ignoring pain sensations.  In other words, there might be
## an enhanced effect with the combination of EX and SM for improving
## a person's sensitivity to pain but only for those who have a
## greater change in positive affect.

summary(csqIPS.aov)

##                  Df Sum Sq Mean Sq F value  Pr(>F)  
## GROUP             2  2.094   1.047  2.0099 0.13684  
## panaspRes         1  0.005   0.005  0.0099 0.92070  
## GROUP:panaspRes   2  4.001   2.001  3.8402 0.02318 *
## Residuals       190 98.986   0.521                  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
## 12 observations deleted due to missingness


summary(lm(csqIPS.aov))

## Call:
## lm(formula = csqIPS.aov)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.70398 -0.50580  0.01731  0.52995  1.82537 

## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)  
## (Intercept)        0.1527     0.0882   1.731   0.0851 .
## GROUP2            -0.2365     0.1279  -1.849   0.0660 .
## GROUP3            -0.1308     0.1268  -1.031   0.3039  
## panaspRes          0.3664     0.2054   1.784   0.0761 .
## GROUP2:panaspRes  -0.2459     0.2623  -0.938   0.3497  
## GROUP3:panaspRes  -0.6440     0.2483  -2.594   0.0102 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 0.7218 on 190 degrees of freedom
##   (12 observations deleted due to missingness)
## Multiple R-Squared: 0.05805,	Adjusted R-squared: 0.03327 
## F-statistic: 2.342 on 5 and 190 DF,  p-value: 0.04311 

#### --------------------------------- ####

# Plot interaction

library(car)

knee.1 <- knee[knee$GROUP==1,]
knee.2 <- knee[knee$GROUP==2,]
knee.3 <- knee[knee$GROUP==3,]

lm.k1 <- lm(csqIPSRes~panaspRes,data=knee.1,na.action=na.exclude)
lm.k2 <- lm(csqIPSRes~panaspRes,data=knee.2,na.action=na.exclude)
lm.k3 <- lm(csqIPSRes~panaspRes,data=knee.3,na.action=na.exclude)

#pdf("DataBlitzSlide02202008.pdf")
plot(csqIPSRes~panaspRes,type="n",data=knee.1,col="blue",pch=20,xlim=c(-2,2),ylim=c(-2,2),xlab="Change in Positive Affect",ylab="Change in Coping Style: Ignorming Pain Sensations")
hv.k1 <- hatvalues(lm.k1)
k1csize <- hv.k1/max(hv.k1)
symbols(knee.1$panaspRes,knee.1$csqIPSRes,circles=k1csize,add=T,bg="blue",inches=.5)
abline(lm.k1,col="blue",lwd=2)

par(new=T)

plot(csqIPSRes~panaspRes,data=knee.2,col="red",pch=20,xlim=c(-2,2),ylim=c(-2,2),axis=F,xlab="",ylab="")
hv.k2 <- hatvalues(lm.k2)
k2csize <- hv.k2/max(hv.k2)
symbols(knee.2$panaspRes,knee.2$csqIPSRes,circles=k2csize,add=T,bg="red",inches=.5)
abline(lm.k2,col="red",lwd=2)

par(new=T)

plot(csqIPSRes~panaspRes,data=knee.3,col="purple",pch=20,xlim=c(-2,2),ylim=c(-2,2),axis=F,xlab="",ylab="")
hv.k3 <- hatvalues(lm.k3)
k3csize <- hv.k3/max(hv.k3)
symbols(knee.3$panaspRes,knee.3$csqIPSRes,circles=k3csize,add=T,bg="purple",inches=.5)
abline(lm.k3,col="purple",lwd=2)

legend(1,-1,legend=c("EX","SM","EX+SM"),col=c("blue","red","purple"),lty=1,lwd=2)
#dev.off()


dev2bitmap("PANAScsqIPSinteraction.png")

#### --------------------------------- ####


csqPH.aov <- aov(csqPHRes~GROUP*panaspRes,knee) # ns
csqCT.aov <- aov(csqCTRes~GROUP*panaspRes,knee)

summary(csqCT.aov)

##                  Df  Sum Sq Mean Sq F value  Pr(>F)  
## GROUP             2  0.0104  0.0052  0.0441 0.95687  
## panaspRes         1  0.7291  0.7291  6.2044 0.01361 *
## GROUP:panaspRes   2  0.1064  0.0532  0.4527 0.63660  
## Residuals       188 22.0930  0.1175                  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
## 14 observations deleted due to missingness

summary(lm(csqCT.aov))

### Where did the effects go?  Multicollinearity ate 'em up!

## Call:
## lm(formula = csqCT.aov)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.48703 -0.15650 -0.09323  0.02447  1.89572 

## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)
## (Intercept)      -0.003434   0.042523  -0.081    0.936
## GROUP2           -0.015155   0.061446  -0.247    0.805
## GROUP3            0.032671   0.060431   0.541    0.589
## panaspRes        -0.039401   0.098014  -0.402    0.688
## GROUP2:panaspRes -0.066155   0.126647  -0.522    0.602
## GROUP3:panaspRes -0.111800   0.118278  -0.945    0.346

## Residual standard error: 0.3428 on 188 degrees of freedom
##   (14 observations deleted due to missingness)
## Multiple R-Squared: 0.03688,	Adjusted R-squared: 0.01126 
## F-statistic:  1.44 on 5 and 188 DF,  p-value: 0.2119 


csqIAL.aov <- aov(csqIALRes~GROUP*panaspRes,knee) # ns

### PLOT MEANS
### Include intercept of PANASP (ns so excluded)


### Tx ---> SELFE (somewhat)

## Self-efficacy 
sepain.aov <- aov(sepainRes ~ GROUP*panaspRes,knee)
## NOTE: both SM and COMBO groups higher than EX group for sepain,
## however, the effect goes away when I added the panasp.

summary(sepain.aov)

##                  Df Sum Sq Mean Sq F value   Pr(>F)   
## GROUP             2  24.68   12.34  2.4355 0.090213 . 
## panaspRes         1  42.24   42.24  8.3376 0.004322 **
## GROUP:panaspRes   2   6.09    3.05  0.6014 0.549056   
## Residuals       195 987.83    5.07                    
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
## 7 observations deleted due to missingness

summary(lm(sepain.aov))

## Call:
## lm(formula = sepain.aov)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.6991 -1.2251  0.4003  1.4734  4.7991 

## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)  
## (Intercept)       -0.5166     0.2731  -1.892   0.0600 .
## GROUP2             0.6883     0.3945   1.745   0.0826 .
## GROUP3             0.6961     0.3925   1.774   0.0777 .
## panaspRes          0.7764     0.5876   1.321   0.1880  
## GROUP2:panaspRes  -0.3451     0.7707  -0.448   0.6548  
## GROUP3:panaspRes   0.3775     0.7309   0.516   0.6061  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 2.251 on 195 degrees of freedom
##   (7 observations deleted due to missingness)
## Multiple R-Squared: 0.06882,	Adjusted R-squared: 0.04494 
## F-statistic: 2.882 on 5 and 195 DF,  p-value: 0.01555 

sefcn.aov <- aov(sefcnRes ~ GROUP*panaspRes,knee) # ns

seosx.aov <- aov(seosxRes ~ GROUP*panaspRes,knee)

summary(seosx.aov)
##                  Df Sum Sq Mean Sq F value    Pr(>F)    
## GROUP             2   5.85    2.93  1.0277  0.359766    
## panaspRes         1  79.15   79.15 27.7871 3.574e-07 ***
## GROUP:panaspRes   2  27.82   13.91  4.8833  0.008523 ** 
## Residuals       195 555.47    2.85                      
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
## 7 observations deleted due to missingness

summary(lm(seosx.aov))

## Call:
## lm(formula = seosx.aov)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.4562 -0.9152  0.3271  1.0363  3.8749 

## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       -0.1504     0.2048  -0.735 0.463450    
## GROUP2             0.1735     0.2958   0.587 0.558156    
## GROUP3             0.2460     0.2943   0.836 0.404171    
## panaspRes          1.6975     0.4406   3.852 0.000159 ***
## GROUP2:panaspRes  -1.5195     0.5779  -2.629 0.009240 ** 
## GROUP3:panaspRes  -0.1519     0.5481  -0.277 0.781996    
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.688 on 195 degrees of freedom
##   (7 observations deleted due to missingness)
## Multiple R-Squared: 0.1688,	Adjusted R-squared: 0.1475 
## F-statistic: 7.922 on 5 and 195 DF,  p-value: 8.151e-07 

## OLD NOTE: without the panasp, there was no effect but when I added
## it in, the panasp was both a significant main effect and a
## significant interaction.  Let me focus on the interaction.  The
## interaction indicates that the SM group shows more negative or less
## change in self-efficacy for other symptoms as positive affect
## increases.


#### --------------------------------- ####

# Plot interaction

knee.1 <- knee[knee$GROUP==1,]
knee.2 <- knee[knee$GROUP==2,]
knee.3 <- knee[knee$GROUP==3,]

lm.k1 <- lm(seosxRes~panaspRes,knee.1)
lm.k2 <- lm(seosxRes~panaspRes,knee.2)
lm.k3 <- lm(seosxRes~panaspRes,knee.3)


plot(seosxRes~panaspRes,data=knee.1,col="blue",pch=20,xlim=c(-2,2),ylim=c(-2,2),xlab="Change in Positive Affect",ylab="Change in Coping Self-Efficacy: Other Symptoms")
abline(lm.k1,col="blue",lwd=2)
par(new=T)
plot(seosxRes~panaspRes,data=knee.2,col="red",pch=20,xlim=c(-2,2),ylim=c(-2,2),axis=F,xlab="",ylab="")
abline(lm.k2,col="red",lwd=2)
par(new=T)
plot(seosxRes~panaspRes,data=knee.3,col="purple",pch=20,xlim=c(-2,2),ylim=c(-2,2),axis=F,xlab="",ylab="")
abline(lm.k3,col="purple",lwd=2)

legend(1,-1,legend=c("EX","SM","EX+SM"),col=c("blue","red","purple"),lty=1,lwd=2)
dev2bitmap("PANASseosxInteraction.png")

#### --------------------------------- ####




### Coping ---> QoL (spotty findings but interesting implications)

csqDA.sf36tot <- lm(sf36totRes ~ csqDARes,knee) # ns
csqDA.sf36pf <-  lm(sf36pfRes ~ csqDARes,knee) # ns
csqDA.sf36bp <-  lm(sf36bpRes ~ csqDAL,knee) # ns
csqDA.eq5d <- lm(eq5dRes ~ csqDARes,knee) # ns

csqRPS.sf36tot <- lm(sf36totRes ~ csqRPSRes,knee) # p=.004, R^2=.28
summary(csqRPS.sf36tot)
plot(sf36totRes ~ csqRPSRes,data=knee,xlab="Change in Coping Style: Reinterpreting Pain Sensations",ylab="Change in SF-36 Total")
abline(csqRPS.sf36tot,col="blue",lwd=2)
dev2bitmap("csqRPSsf36tot.png")

## Call:
## lm(formula = sf36totRes ~ csqRPSRes, data = knee)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7686 -0.6130  0.1343  0.8062  3.4660 

## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -0.009048   0.088955  -0.102  0.91909   
## csqRPSRes   -0.352080   0.118362  -2.975  0.00330 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.249 on 195 degrees of freedom
##   (11 observations deleted due to missingness)
## Multiple R-Squared: 0.04341,	Adjusted R-squared: 0.0385 
## F-statistic: 8.848 on 1 and 195 DF,  p-value: 0.003304 

csqRPS.sf36pf <-  lm(sf36pfRes ~ csqRPSRes,knee) # p=.01,R^2=.03
summary(csqRPS.sf36pf)
plot(sf36pfRes ~ csqRPSRes,data=knee,xlab="Change in Coping Style: Reinterpreting Pain Sensations",ylab="Change in SF-36 Physical Functioning")
abline(csqRPS.sf36pf,col="blue",lwd=2)
dev2bitmap("csqRPSsf36pf.png")

## Call:
## lm(formula = sf36pfRes ~ csqRPSRes, data = knee)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.9450 -0.8543  0.1654  0.9681  4.4358 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  0.01735    0.11323   0.153   0.8784  
## csqRPSRes   -0.37715    0.15099  -2.498   0.0133 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.593 on 196 degrees of freedom
##   (10 observations deleted due to missingness)
## Multiple R-Squared: 0.03085,	Adjusted R-squared: 0.02591 
## F-statistic: 6.239 on 1 and 196 DF,  p-value: 0.01332 



## NOTE: The greater the change in reinterpreting change sensations,
## the more negative the SF36 physical functioning change


csqRPS.sf36bp <-  lm(sf36bpRes ~ csqRPSRes,knee) # ns
csqRPS.eq5d <- lm(eq5dRes ~ csqRPSRes,knee) # ns

csqCSS.sf36tot <- lm(sf36totRes ~ csqCSSRes,knee) # ns
csqCSS.sf36pf <-  lm(sf36pfRes ~ csqCSSRes,knee) # ns
csqCSS.sf36bp <-  lm(sf36bpRes ~ csqCSSRes,knee) # ns
csqCSS.eq5d <- lm(eq5dRes ~ csqCSSRes,knee) # ns

csqIPS.sf36tot <- lm(sf36totRes ~ csqIPSRes,knee) # ns
csqIPS.sf36pf <-  lm(sf36pfRes ~  csqIPSRes,knee) # ns
csqIPS.sf36bp <-  lm(sf36bpRes ~ csqIPSRes,knee) # ns
csqIPS.eq5d <- lm(eq5dRes ~ csqIPSRes,knee) # ns

csqPH.sf36tot <- lm(sf36totRes ~ csqPHRes,knee) # ns
csqPH.sf36pf <-  lm(sf36pfRes ~ csqPHRes,knee) # ns
csqPH.sf36bp <-  lm(sf36bpRes ~ csqPHRes,knee) # ns
csqPH.eq5d <- lm(eq5dRes ~ csqPHRes,knee) # ns

csqCT.sf36tot <- lm(sf36totRes ~ csqCTRes,knee) # p=.0004 R^2=.09
summary(csqCT.sf36tot)
plot(sf36totRes ~ csqCTRes,data=knee,xlab="Change in Coping Style: Catastrophizing",ylab="Change in SF-36 Total")
abline(csqCT.sf36tot,col="blue",lwd=2)
dev2bitmap("csqCTsf36tot.png")

## Call:
## lm(formula = sf36totRes ~ csqCTRes, data = knee)

## Residuals:
##      Min       1Q   Median       3Q      Max 
## -4.65051 -0.58841  0.08884  0.77494  2.87418 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.01926    0.08605  -0.224    0.823    
## csqCTRes    -1.17913    0.25780  -4.574  8.5e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.208 on 195 degrees of freedom
##   (11 observations deleted due to missingness)
## Multiple R-Squared: 0.09689,	Adjusted R-squared: 0.09225 
## F-statistic: 20.92 on 1 and 195 DF,  p-value: 8.503e-06 

csqCT.sf36pf <-  lm(sf36pfRes ~ csqCTRes,knee)
summary(csqCT.sf36pf)
plot(sf36pfRes ~ csqCTRes,data=knee,xlab="Change in Coping Style: Catastrophizing",ylab="Change in SF-36 Physical Functioning")
abline(csqCT.sf36pf,col="blue",lwd=2)
dev2bitmap("csqCTsf36pf.png")

## Call:
## lm(formula = sf36pfRes ~ csqCTRes, data = knee)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.1570 -0.9806  0.2039  1.0628  4.4913 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -0.02447    0.11532  -0.212  0.83220   
## csqCTRes    -1.08871    0.33819  -3.219  0.00150 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.623 on 196 degrees of freedom
##   (10 observations deleted due to missingness)
## Multiple R-Squared: 0.05022,	Adjusted R-squared: 0.04537 
## F-statistic: 10.36 on 1 and 196 DF,  p-value: 0.001505 


csqCT.sf36bp <-  lm(sf36bpRes ~ csqCTRes,knee)
summary(csqCT.sf36bp)
plot(sf36bpRes ~ csqCTRes,data=knee,xlab="Change in Coping Style: Catastrophizing",ylab="Change in SF-36 Bodily Pain")
abline(csqCT.sf36bp,col="blue",lwd=2)
dev2bitmap("csqCTsf36bp.png")

## Call:
## lm(formula = sf36bpRes ~ csqCTRes, data = knee)

## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.3613 -1.0268  0.2430  1.2171  3.9034 

## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.04303    0.12468  -0.345     0.73    
## csqCTRes    -1.69881    0.37353  -4.548  9.5e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 1.75 on 195 degrees of freedom
##   (11 observations deleted due to missingness)
## Multiple R-Squared: 0.0959,	Adjusted R-squared: 0.09126 
## F-statistic: 20.68 on 1 and 195 DF,  p-value: 9.502e-06 


csqCT.eq5d <- lm(eq5dRes ~ csqCTRes,knee)
summary(csqCT.eq5d)
plot(eq5dRes ~ csqCTRes,data=knee,xlab="Change in Coping Style: Catastrophizing",ylab="Change in EQ-5D")
abline(csqCT.eq5d,col="blue",lwd=2)
dev2bitmap("csqCTeq5d.png")

## Call:
## lm(formula = eq5dRes ~ csqCTRes, data = knee)

## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.043268 -0.004849 -0.002062  0.007309  0.023435 

## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)   
## (Intercept) -6.142e-05  7.051e-04  -0.087  0.93067   
## csqCTRes    -5.558e-03  2.061e-03  -2.696  0.00765 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## Residual standard error: 0.009744 on 189 degrees of freedom
##   (17 observations deleted due to missingness)
## Multiple R-Squared: 0.03704,	Adjusted R-squared: 0.03195 
## F-statistic:  7.27 on 1 and 189 DF,  p-value: 0.007645 


### NOTE: The greater the change in catastrophizing, the more negative
### the change in QoL

csqIAL.sf36tot <- lm(sf36totRes ~ csqIALRes,knee) # ns
csqIAL.sf36pf <-  lm(sf36pfRes ~ csqIALRes,knee) # ns
csqIAL.sf36bp <-  lm(sf36bpRes ~ csqIALRes,knee) # ns
csqIAL.eq5d <- lm(eq5dRes ~ csqIALRes,knee) # ns


### Self-efficacy ---> QoL (strong findings)

sepain.sf36tot <- lm(sf36totL ~ sf36totI + sepainL,knee) # p=.004 R^2=.27
sepain.sf36pf <-  lm(sf36pfL ~ sf36pfI + sepainL,knee) # ns
sepain.sf36bp <-  lm(sf36bpL ~ sf36bpI + sepainL,knee) # p=8x10-5,R^2=.38
sepain.eq5d <- lm(eq5dL ~ eq5dI + sepainL,knee) # p=.004 R^2=.37
## NOTE: the greater the change in pain self-efficacy, the greater the
## change in QoL

sefcn.sf36tot <- lm(sf36totL ~ sf36totI + sefcnL,knee) # p=.0001 R^2=.29
sefcn.sf36pf <-  lm(sf36pfL ~ sf36pfI + sefcnL,knee) # p=.02 R^2=.31
sefcn.sf36bp <-  lm(sf36bpL ~ sf36bpI + sefcnL,knee) # p=2.4x10-4 R^2=.28
sefcn.eq5d <- lm(eq5dL ~ eq5dI + sefcnL,knee) # p=.004 R^2=.37
## NOTE: the greater the change in functioning self-efficacy, the
## greater the change in QoL

seosx.sf36tot <- lm(sf36totL ~ sf36totI + seosxL,knee) # p=1.8x10-6 R^2=.32
seosx.sf36pf <-  lm(sf36pfL ~ sf36pfI + seosxL,knee) # ns
seosx.sf36bp <-  lm(sf36bpL ~ sf36bpI + seosxL,knee) # p=4x10-5 R^2=.38
seosx.eq5d <- lm(eq5dL ~ eq5dI + seosxL,knee) # p=.0004 R^2=.38
## NOTE: the greater the change in other symptom self-efficacy, the
## greater the change in QoL

## ZZZZ

### Coping ---> Pain (spotty)
csqDA.kpainvas <- lm(kpainvasL ~ kpainvasI + csqDAL,knee) # ns
csqRPS.kpainvas <- lm(kpainvasL ~ kpainvasI + csqRPSL,knee) # ns
csqCSS.kpainvas <- lm(kpainvasL ~ kpainvasI + csqCSSL,knee) # ns
csqIPS.kpainvas <- lm(kpainvasL ~ kpainvasI + csqIPSL,knee) # ns
csqPH.kpainvas <- lm(kpainvasL ~ kpainvasI + csqPHL,knee) # ns
csqCT.kpainvas <- lm(kpainvasL ~ kpainvasI + csqCTL,knee) #p=.004 R^2=.41
## NOTE: as coping via catastrophizing increases, reported pain
## increases
csqIAL.kpainvas <- lm(kpainvasL ~ kpainvasI + csqIALL,knee) # ns


### Self-efficacy ---> Pain (strongly related as expected)
sepain.kpainvas <- lm(kpainvasL ~ kpainvasI + sepainL,knee) #p=.001 R^2=.42
sefcn.kpainvas <- lm(kpainvasL ~ kpainvasI + sefcnL,knee) #ns
seosx.kpainvas <- lm(kpainvasL ~ kpainvasI + seosxL,knee) # p=.0006 R^2=.42
## NOTE: the greater the change in coping with pain or other symptoms,
## the more negative the slope in pain reporting.


### Pain ---> QoL (strong relationship as expected)
kpainvas.sf36tot <- lm(sf36totL ~ sf36totI + kpainvasL,knee) # p=2x10-7 R^2=.32
kpainvas.sf36pf <-  lm(sf36pfL ~ sf36pfI + kpainvasL,knee) # p=.003 R^2=.32
kpainvas.sf36bp <-  lm(sf36bpL ~ sf36bpI + kpainvasL,knee) # p=2x10-8 R^2=.42
kpainvas.eq5d <- lm(eq5dL ~ eq5dI + kpainvasL,knee) # p=.0002 R^2=.39

#################################################
###
### Alex Afram's CT mediation analysis
###
#################################################

##       c
## CT -------> PF
##  \        ^
##   \a     /b
##    \    /
##     \  /
##      SE

## check temporal order between CT and SE
## use KNEEf data

KNEEf.0 <- KNEEf[KNEEf$month==0,c("personid","CT","Pain","Function","OtherSx","CESDtotal","KNEEPAINVAS","ARTHRITISVAS","AGE","SEX","pain","stiffness","disability")]
KNEEf.3 <- KNEEf[KNEEf$month==3,c("personid","CT","Pain","Function","OtherSx","CESDtotal","KNEEPAINVAS","ARTHRITISVAS","AGE","SEX","pain","stiffness","disability")]
KNEEf.9 <- KNEEf[KNEEf$month==9,c("personid","CT","Pain","Function","OtherSx","CESDtotal","KNEEPAINVAS","ARTHRITISVAS","AGE","SEX","pain","stiffness","disability")]

names(KNEEf.0) <- c("personid","CT.0","Pain.0","Function.0","OtherSx.0","CESDtotal.0","KNEEPAINVAS.0","ARTHRITISVAS.0","AGE.0","SEX.0","pain.0","stiffness.0","disability.0")
names(KNEEf.3) <- c("personid","CT.3","Pain.3","Function.3","OtherSx.3","CESDtotal.3","KNEEPAINVAS.3","ARTHRITISVAS.3","AGE.3","SEX.3","pain.3","stiffness.3","disability.3")
names(KNEEf.9) <- c("personid","CT.9","Pain.9","Function.9","OtherSx.9","CESDtotal.9","KNEEPAINVAS.9","ARTHRITISVAS.9","AGE.9","SEX.9","pain.9","stiffness.9","disability.9")

KNEEf.0 <- KNEEf.0[!is.na(KNEEf.0$personid),]
KNEEf.3 <- KNEEf.3[!is.na(KNEEf.3$personid),]
KNEEf.9 <- KNEEf.9[!is.na(KNEEf.9$personid),]

KNEEf.0$SE.0 <- zfa(KNEEf.0[,c("Pain.0","Function.0","OtherSx.0")],use="pairwise.complete.obs")$scores
KNEEf.3$SE.3 <- zfa(KNEEf.3[,c("Pain.3","Function.3","OtherSx.3")],use="pairwise.complete.obs")$scores
KNEEf.9$SE.9 <- zfa(KNEEf.9[,c("Pain.9","Function.9","OtherSx.9")],use="pairwise.complete.obs")$scores

KNEEf.0$Aseverity.0 <- zfa(KNEEf.0[,c("KNEEPAINVAS.0","ARTHRITISVAS.0","pain.0","stiffness.0","disability.0")],use="pairwise.complete.obs")$scores
KNEEf.3$Aseverity.3 <- zfa(KNEEf.3[,c("KNEEPAINVAS.3","ARTHRITISVAS.3","pain.3","stiffness.3","disability.3")],use="pairwise.complete.obs")$scores
KNEEf.9$Aseverity.9 <- zfa(KNEEf.9[,c("KNEEPAINVAS.9","ARTHRITISVAS.9","pain.9","stiffness.9","disability.9")],use="pairwise.complete.obs")$scores

KNEEf.0$CT.res.0 <- resid(lm(CT.0~Aseverity.0+AGE.0+SEX.0+CESDtotal.0,data=KNEEf.0,na.action=na.exclude))
KNEEf.3$CT.res.3 <- resid(lm(CT.3~Aseverity.3+AGE.3+SEX.3,data=KNEEf.3,na.action=na.exclude))
KNEEf.9$CT.res.9 <- resid(lm(CT.9~Aseverity.9+AGE.9+SEX.9+CESDtotal.9,data=KNEEf.9,na.action=na.exclude))

KNEEf.0$SE.res.0 <- resid(lm(SE.0~Aseverity.0+AGE.0+SEX.0+CESDtotal.0,data=KNEEf.0,na.action=na.exclude))
KNEEf.3$SE.res.3 <- resid(lm(SE.3~Aseverity.3+AGE.3+SEX.3,data=KNEEf.3,na.action=na.exclude))
KNEEf.9$SE.res.9 <- resid(lm(SE.9~Aseverity.9+AGE.9+SEX.9+CESDtotal.9,data=KNEEf.9,na.action=na.exclude))

KNEEtempord <- merge(KNEEf.0,KNEEf.3,all=T,by="personid")
KNEEtempord <- merge(KNEEtempord,KNEEf.9,all=T,by="personid")

KNEEtempord$CT.3.R <- resid(lm(CT.res.3~CT.res.0,data=KNEEtempord,na.action=na.exclude))
KNEEtempord$CT.9.R <- resid(lm(CT.res.9~CT.res.3,data=KNEEtempord,na.action=na.exclude))

KNEEtempord$SE.3.R <- resid(lm(SE.res.3~SE.res.0,data=KNEEtempord,na.action=na.exclude))
KNEEtempord$SE.9.R <- resid(lm(SE.res.9~SE.res.3,data=KNEEtempord,na.action=na.exclude))

orderCor <- cor(KNEEtempord[,c("CT.res.0","CT.3.R","CT.9.R")],KNEEtempord[,c("SE.res.0","SE.3.R","SE.9.R")],use="pairwise.complete.obs")

library(lattice)

grpXYPlot <- function(x,dv){
  DV <- x[,dv]
  xyplot(DV~month | GLabel, x, groups=personid, type='p',auto.key=F,ylab=dv,panel=function(...){
    panel.loess(...,col='black',lwd=3)
    panel.superpose(...)
  })
}

## test cross lagged design per our conversation at MRES on 6/12 to determine causal order

CTSEcl.dat <- KNEEtempord[,c("CT.res.0","CT.res.3","CT.res.9","SE.res.0","SE.res.3","SE.res.9")]

names(CTSEcl.dat) <- c("CT0","CT3","CT9","SE0","SE3","SE9")

CTSEcl.mice <- mice(CTSEcl.dat)

CTSEcl.cov.1 <- cov(complete(CTSEcl.mice,1))
CTSEcl.cov.2 <- cov(complete(CTSEcl.mice,2))
CTSEcl.cov.3 <- cov(complete(CTSEcl.mice,3))
CTSEcl.cov.4 <- cov(complete(CTSEcl.mice,4))
CTSEcl.cov.5 <- cov(complete(CTSEcl.mice,5))

CTSEcl.model <- matrix(c('SE0 -> SE3','b1',NA,
                         'SE3 -> SE9','b2',NA,
                         'CT0 -> CT3','b3',NA,
                         'CT3 -> CT9','b4',NA,
                         'SE0 <-> SE0','th1',NA,
                         'SE3 <-> SE3','th1',NA,
                         'SE9 <-> SE9','th1',NA,
                         'CT0 <-> CT0','th2',NA,
                         'CT3 <-> CT3','th2',NA,
                         'CT9 <-> CT9','th2',NA,
                         'CT0 <-> SE0','th3',NA,
                         'CT3 <-> SE3','th4',NA,
                         'CT9 <-> SE9','th5',NA,
                         'CT0 -> SE3','b5',NA,
                         'SE0 -> CT3','b6',NA,
                         'CT3 -> SE9','b7',NA,
                         'SE3 -> CT9','b8',NA),ncol=3,byrow=T)

CTtoSE.model <- matrix(c('SE0 -> SE3','b1',NA,
                         'SE3 -> SE9','b2',NA,
                         'CT0 -> CT3','b3',NA,
                         'CT3 -> CT9','b4',NA,
                         'SE0 <-> SE0','th1',NA,
                         'SE3 <-> SE3','th1',NA,
                         'SE9 <-> SE9','th1',NA,
                         'CT0 <-> CT0','th2',NA,
                         'CT3 <-> CT3','th2',NA,
                         'CT9 <-> CT9','th2',NA,
                         'CT0 <-> SE0','th3',NA,
                         'CT3 <-> SE3','th4',NA,
                         'CT9 <-> SE9','th5',NA,
                         'CT0 -> SE3','b5',NA,
                         'CT3 -> SE9','b7',NA),ncol=3,byrow=T)

SEtoCT.model <- matrix(c('SE0 -> SE3','b1',NA,
                         'SE3 -> SE9','b2',NA,
                         'CT0 -> CT3','b3',NA,
                         'CT3 -> CT9','b4',NA,
                         'SE0 <-> SE0','th1',NA,
                         'SE3 <-> SE3','th1',NA,
                         'SE9 <-> SE9','th1',NA,
                         'CT0 <-> CT0','th2',NA,
                         'CT3 <-> CT3','th2',NA,
                         'CT9 <-> CT9','th2',NA,
                         'CT0 <-> SE0','th3',NA,
                         'CT3 <-> SE3','th4',NA,
                         'CT9 <-> SE9','th5',NA,
                         'SE0 -> CT3','b6',NA,
                         'SE3 -> CT9','b8',NA),ncol=3,byrow=T)

CTSEcl.sem <- sem(CTSEcl.model,CTSEcl.cov.1,261)
CTtoSE.sem <- sem(CTtoSE.model,CTSEcl.cov.1,261)
SEtoCT.sem <- sem(SEtoCT.model,CTSEcl.cov.1,261)

std.coef(CTSEcl.sem)
std.coef(CTtoSE.sem)
std.coef(SEtoCT.sem)

anova(CTSEcl.sem,CTtoSE.sem)
anova(CTSEcl.sem,SEtoCT.sem)


## modification indices

CTSEcl.model2 <- matrix(c('SE0 -> SE3','b1',NA,
                         'SE3 -> SE9','b2',NA,
                         'CT0 -> CT3','b3',NA,
                         'CT3 -> CT9','b4',NA,
                         'CT0 -> CT9','b9',NA,
                         'SE0 -> SE9','b10',NA,
                         'SE0 <-> SE0','th1',NA,
                         'SE3 <-> SE3','th1',NA,
                         'SE9 <-> SE9','th1',NA,
                         'CT0 <-> CT0','th2',NA,
                         'CT3 <-> CT3','th3',NA,
                         'CT9 <-> CT9','th4',NA,
                         'CT0 <-> SE0','th5',NA,
                         'CT3 <-> SE3','th6',NA,
                         'CT9 <-> SE9','th7',NA,
                         'CT0 -> SE3','b5',NA,
                         'SE0 -> CT3','b6',NA,
                         'CT3 -> SE9','b7',NA,
                         'SE3 -> CT9','b8',NA),ncol=3,byrow=T)

CTtoSE.model2 <- matrix(c('SE0 -> SE3','b1',NA,
                          'SE3 -> SE9','b2',NA,
                          'SE0 -> SE9','b3',NA,
                          'CT0 -> CT3','b4',NA,
                          'CT3 -> CT9','b5',NA,
                          'CT0 -> CT9','b6',NA,
                          'CT0 -> SE3','b7',NA,
                          'CT0 -> SE9','b8',NA,
                          'SE0 <-> SE0','th1',NA,
                          'SE3 <-> SE3','th1',NA,
                          'SE9 <-> SE9','th1',NA,
                          'CT0 <-> CT0','th2',NA,
                          'CT3 <-> CT3','th2',NA,
                          'CT9 <-> CT9','th2',NA,
                          'CT0 <-> SE0','th3',NA,
                          'CT3 <-> SE3','th4',NA,
                          'CT9 <-> SE9','th5',NA),ncol=3,byrow=T)

SEtoCT.model2 <- matrix(c('SE0 -> SE3','b1',NA,
                          'SE3 -> SE9','b2',NA,
                          'SE0 -> SE9','b3',NA,
                          'CT0 -> CT3','b4',NA,
                          'CT3 -> CT9','b5',NA,
                          'CT0 -> CT9','b6',NA,
                          'SE0 -> CT3','b7',NA,
                          'SE3 -> CT9','b8',NA,
                          'SE0 <-> SE0','th1',NA,
                          'SE3 <-> SE3','th1',NA,
                          'SE9 <-> SE9','th1',NA,
                          'CT0 <-> CT0','th2',NA,
                          'CT3 <-> CT3','th2',NA,
                          'CT9 <-> CT9','th2',NA,
                          'CT0 <-> SE0','th3',NA,
                          'CT3 <-> SE3','th4',NA,
                          'CT9 <-> SE9','th5',NA),ncol=3,byrow=T)

CTSEcl.model3 <- matrix(c('SE0 -> SE3','b1',NA,
                          'SE3 -> SE9','b2',NA,
                          'SE0 -> SE9','b3',NA,
                          'CT0 -> CT3','b4',NA,
                          'CT3 -> CT9','b5',NA,
                          'CT0 -> CT9','b6',NA,
                          'CT0 -> SE3','b7',NA,
                         'SE0 <-> SE0','th1',NA,
                         'SE3 <-> SE3','th2',NA,
                         'SE9 <-> SE9','th3',NA,
                         'CT0 <-> CT0','th4',NA,
                         'CT3 <-> CT3','th5',NA,
                         'CT9 <-> CT9','th6',NA,
                         'CT0 <-> SE0','th7',NA,
                         'CT3 <-> SE3','th8',NA,
                         'CT9 <-> SE9','th9',NA),ncol=3,byrow=T)



CTSEcl.sem2 <- sem(CTSEcl.model2,CTSEcl.cov.1,261)
CTtoSE.sem2 <- sem(CTtoSE.model2,CTSEcl.cov.1,261)
SEtoCT.sem2 <- sem(SEtoCT.model2,CTSEcl.cov.1,261)

CTSEcl.sem3 <- sem(CTSEcl.model3,CTSEcl.cov.1,261)


# CT change process by group
library(nlme)
ct.dat <- KNEEf9[complete.cases(KNEEf9[,c("personid","CT","month","GROUP")]),c("personid","CT","month","GROUP","GLabel")]
ct.dat$invCT <- ct.dat$CT^-1
CT.grpd <- groupedData(CT~month | GLabel,data=ct.dat)

plot(CT.grpd)

grpXYPlot(ct.dat,"CT")

# SE change process by group

se.dat <- KNEEf9[complete.cases(KNEEf9[,c("personid","Pain","Function","OtherSx","month","GROUP")]),c("personid","Pain","Function","OtherSx","month","GROUP","GLabel")]
SEpain.grpd <- groupedData(Pain~month | personid,outer=~GLabel,data=se.dat)
plot(SEpain.grpd,outer=T)

grpXYPlot(se.dat,"Pain")


SEfunc.grpd <- groupedData(Function~month | GLabel,data=se.dat)
plot(SEfunc.grpd)

grpXYPlot(se.dat,"Function")

SEosx.grpd <- groupedData(OtherSx~month | GLabel,data=se.dat)
plot(SEosx.grpd)

grpXYPlot(se.dat,"OtherSx")

se.dat$SE.all <- rowSums(se.dat[,2:4])
SEall.grpd <- groupedData(SE.all~month | GLabel,data=se.dat)

pdf("SelfEfficacyBYtreatment.pdf")
plot(SEall.grpd)
dev.off()


# PF change process by group

pf.dat <- KNEEf9[complete.cases(KNEEf9[,c("personid","PhysicalFunc","month","GROUP")]),c("personid","PhysicalFunc","month","GROUP","GLabel")]
PF.grpd <- groupedData(PhysicalFunc~month | GLabel,data=pf.dat)
plot(PF.grpd)


####### Mediation tests by piecemeal

## CT to SE (a)

alex.lm1 <- lm(sepainRes~csqCTRes,data=knee)
alex.lm1a <- lm(scale(sepainRes)~scale(csqCTRes) -1,data=knee)

alex.lm2 <- lm(sefcnRes~csqCTRes,data=knee)
alex.lm2a <- lm(scale(sefcnRes)~scale(csqCTRes) -1,data=knee)

alex.lm3 <- lm(seosxRes~csqCTRes,data=knee)
alex.lm3a <- lm(scale(seosxRes)~scale(csqCTRes) -1,data=knee)

## summary: average reg. coef = .31 (sig p < .05)

## SE to PF (b)

alex.lm4 <- lm(sf36pfRes~sepainRes,data=knee)
alex.lm4a <- lm(sf36pfRes~scale(sepainRes)-1,data=knee)

alex.lm5 <- lm(sf36pfRes~sefcnRes,data=knee)
alex.lm5a <- lm(sf36pfRes~scale(sefcnRes)-1,data=knee)

alex.lm6 <- lm(sf36pfRes~seosxRes,data=knee)
alex.lm6a <- lm(sf36pfRes~scale(seosxRes)-1,data=knee)

## CT to PF (c)

alex.lm7 <- lm(sf36pfRes~csqCTRes,data=knee)
alex.lm7a <- lm(sf36pfRes~scale(csqCTRes)-1,data=knee)

## final model to test mediation

alex.lm8 <- lm(sf36pfRes~csqCTRes + sepainRes,data=knee)
alex.lm8a <- lm(scale(sf36pfRes)~scale(csqCTRes) + scale(sepainRes),data=knee)

alex.lm9 <- lm(sf36pfRes~csqCTRes + sefcnRes,data=knee)
alex.lm9a <- lm(scale(sf36pfRes)~scale(csqCTRes) + scale(sefcnRes),data=knee)

alex.lm10 <- lm(sf36pfRes~csqCTRes + seosxRes,data=knee)
alex.lm10a <- lm(scale(sf36pfRes)~scale(csqCTRes) + scale(seosxRes),data=knee)


### odd observations that are strongly influencing these results

#### Growth curve data for CAT-SE paper:

# first residualize out confounds

cov1.lm <- lm(Pain~ARTHRITISVAS + SEX + AGE,data=KNEEf9,na.action=na.exclude)
cov2.lm <- lm(Function~ARTHRITISVAS + SEX + AGE,data=KNEEf9,na.action=na.exclude)
cov3.lm <- lm(OtherSx~ARTHRITISVAS + SEX + AGE,data=KNEEf9,na.action=na.exclude)
cov4.lm <- lm(CT~ARTHRITISVAS + SEX + AGE,data=KNEEf9,na.action=na.exclude)
cov5.lm <- lm(PhysicalFunc~ARTHRITISVAS + SEX + AGE,data=KNEEf9,na.action=na.exclude)

KNEEf9$PainRES <- resid(cov1.lm)
KNEEf9$FunctionRES <- resid(cov2.lm)
KNEEf9$OtherSxRES <- resid(cov3.lm)
KNEEf9$CTRES <- resid(cov4.lm)
KNEEf9$PhysicalFuncRES <- resid(cov5.lm)

# second, compute growth curves

SEpain9.igc <- igc(KNEEf9,"personid","month","PainRES")
SEfunc9.igc <- igc(KNEEf9,"personid","month","FunctionRES")
SEosx9.igc <- igc(KNEEf9,"personid","month","OtherSxRES")
CT9.igc <- igc(KNEEf9,"personid","month","CTRES")
PF9.igc <- igc(KNEEf9,"personid","month","PhysicalFuncRES")

# third, extract gc params

SEpain9 <- extractigc(SEpain9.igc,"sepain")
SEfunc9 <- extractigc(SEfunc9.igc,"sefunc")
SEosx9 <- extractigc(SEosx9.igc,"seosx")
CT9 <- extractigc(CT9.igc,"CT")
PF9 <- extractigc(PF9.igc,"PF")

# fourth, merge gc params together

DepBL <- KNEEf9[KNEEf9$month==0,c("personid","CESDtotal")]
alex.igcs <- merge(SEpain9,SEfunc9,by="personid",all=T)
alex.igcs <- merge(alex.igcs,SEosx9,by="personid",all=T)
alex.igcs <- merge(alex.igcs,CT9,by="personid",all=T)
alex.igcs <- merge(alex.igcs,PF9,by="personid",all=T)
alex.igcs <- merge(alex.igcs,DepBL,by="personid",all=T)

alex.igcs <- merge(alex.igcs,PERSON[,c(1,2,3,4)],by.x="personid",by.y="PERSONID")
alex.igcs <- merge(alex.igcs,pemKEY,all=T,by="SUBGROUPID")

alex.igcs <- alex.igcs[!is.na(alex.igcs$personid),]

## residualize out intercepts from slopes and get rid of CESD before aggregating

si1.lm <- lm(alex.igcs[,4]~alex.igcs[,3],na.action=na.exclude)
si2.lm <- lm(alex.igcs[,9]~alex.igcs[,8],na.action=na.exclude)
si3.lm <- lm(alex.igcs[,14]~alex.igcs[,13],na.action=na.exclude)
si4.lm <- lm(alex.igcs[,19]~alex.igcs[,18] + alex.igcs[,28],na.action=na.exclude)
si5.lm <- lm(alex.igcs[,24]~alex.igcs[,23],na.action=na.exclude)

siALL.meanAdjRsq <- mean(summary(si1.lm)$adj.r.squared,summary(si2.lm)$adj.r.squared,summary(si3.lm)$adj.r.squared,summary(si4.lm)$adj.r.squared,summary(si5.lm)$adj.r.squared)

alex.igcs$SEpain9Res <- resid(si1.lm)
alex.igcs$SEfunc9Res <- resid(si2.lm)
alex.igcs$SEosx9Res <- resid(si3.lm)
alex.igcs$CT9Res <- resid(si4.lm)
alex.igcs$PF9Res <- resid(si5.lm)

alex.dat <- merge(alex.igcs,KNEEstatic[,c(1,3,4)],all.y=T,by="personid")

write.csv(alex.dat,"./FinalData/KNEECATSE.csv",row.names=FALSE)

knee.alex <- alex.dat[,c("GROUP","PF9Res","CT9Res","SEpain9Res","SEfunc9Res","SEosx9Res")]
knee.alex <- knee.alex[complete.cases(knee.alex),]


### Test growth curve models via linear, mixed effects model to ensure
### slopes are adequate outcomes

## SEpain
sep.dat <- KNEEf9[,c("personid","month","PainRES")]
sep.dat <- sep.dat[complete.cases(sep.dat),]
sep.grpd <- groupedData(PainRES~month|personid,data=KNEEf9)
plot(sep.grpd)
sep1 <- lme(sep.grpd)

## SEfcn

## SEosx

## CT

## PF





### NOTE: last two items on the CSQ (CSQ43 and CSQ44) are
### self-efficacy items as well.  We might consider combining the
### arthritis self-efficacy items along with those items and get a
### more relable estimate of coping self-efficacy.
###
### TTD: run the model in an SEM multisample analysis by treatment
### group and include the ALMA data as well as a fourth group.
### Constrain the model according to the mediational model above.


names(knee.alex) <- c("Group","PF","CT","SEpain","SEfcn","SEosx")

alex.cov <- cov(knee.alex[,-1],use="pairwise.complete.obs")

## attempt SEM multisample in R

library(sem)

kneeA1.model <- matrix(c('SE -> SEpain','lam11',NA,
                         'SE -> SEfcn','lam12',NA,
                         'SE -> SEosx','lam13',NA,
                         'SE <-> SE',NA,1,
                         'CT -> SE','gam1',NA,
                         'SE -> PF','gam2',NA,
                         'CT -> PF','gam3',NA,
                         'SEpain <-> SEpain','th1',NA,
                         'SEfcn <-> SEfcn','th2',NA,
                         'SEosx <-> SEosx','th3',NA,
                         'CT <-> CT','th4',NA,
                         'PF <-> PF','th5',NA),ncol=3,byrow=T)

kneeA1.sem <- sem(kneeA1.model,alex.cov,211)

std.coef(kneeA1.sem)

kneeA2.model <- matrix(c('SE -> SEpain','lam11',NA,
                         'SE -> SEfcn','lam12',NA,
                         'SE -> SEosx','lam13',NA,
                         'SE <-> SE',NA,1,
                         'SE -> CT','gam1',NA,
                         'CT -> PF','gam2',NA,
                         'SE -> PF','gam3',NA,
                         'SEpain <-> SEpain','th1',NA,
                         'SEfcn <-> SEfcn','th2',NA,
                         'SEosx <-> SEosx','th3',NA,
                         'CT <-> CT','th4',NA,
                         'PF <-> PF','th5',NA),ncol=3,byrow=T)

kneeA2.sem <- sem(kneeA2.model,alex.cov,211)

std.coef(kneeA2.sem)


#########################################################
###
### end Alex Afram's mediational model
###
#########################################################



### reduce data and analyze in an SEM manner

## NOTE: variables are the residual variables from the IGC -
## residualized out intercepts so the variance represents pure change.

knee2 <- knee[,c(100,102:120,122,123)]

## knee2.mids <- mice(knee2) ## singular

knee2$SM <- 0
knee2$BO <- 0
knee2$SM[knee2$GROUP==2] <- 1
knee2$BO[knee2$GROUP==3] <- 1

knee2$coping <- zfa(knee2[,4:10])$scores
knee2$selfe <- zfa(knee2[,15:17])$scores
knee2$fcn <- zfa(knee2[,c(11:14,20)])$scores


cor(knee2,use="pairwise.complete.obs")

library(sem)

knee2.model <- matrix(c('GROUP -> coping','b1',NA,
                       'GROUP -> selfe','b2',NA,
                       'coping -> fcn','b3',NA,
                       'selfe -> fcn','b4',NA,
                        'GROUP <-> GROUP','e1',NA,
                        'coping <-> coping','e2',NA,
                        'selfe <-> selfe','e3',NA,
                        'fcn <-> fcn','e4',NA),ncol=3,byrow=T)

knee2.cov <- cov(knee2[,c(1,25:27)],use="pairwise.complete.obs")

knee2.sem <- sem(knee2.model,knee2.cov,208)

##  Model Chisquare =  4.3068   Df =  2 Pr(>Chisq) = 0.11609
##  Chisquare (null model) =  73.795   Df =  6
##  Goodness-of-fit index =  0.9898
##  Adjusted goodness-of-fit index =  0.94896
##  RMSEA index =  0.074646   90% CI: (NA, 0.17363)
##  Bentler-Bonnett NFI =  0.94164
##  Tucker-Lewis NNFI =  0.89792
##  Bentler CFI =  0.96597
##  SRMR =  0.049448
##  BIC =  -6.3683 

##  Normalized Residuals
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## -1.94e+00 -2.45e-01  8.99e-17 -2.73e-01  1.08e-01  5.58e-01 

##  Parameter Estimates
##    Estimate  Std Error z value Pr(>|z|)                   
## b1  0.038351 0.057793   0.6636 0.506946 coping <--- GROUP 
## b2  0.130411 0.064764   2.0136 0.04Adobe Reader 84049 selfe <--- GROUP  
## b3 -0.089941 0.074047  -1.2146 0.224501 fcn <--- coping   
## b4  0.552169 0.065509   8.4290 0panasp.igc <- igc(KNEEf,"personid","month","positive").000000 fcn <--- selfe    
## e1  0.666574 0.065540  10.1704 0.000000 GROUP <--> GROUP  
## e2  0.460856 0.045319  10.1691 0.000000 coping <--> coping
## e3  0.578743 0.056907  10.1700 0.000000 selfe <--> selfe  
## e4  0.515548 0.050695  10.1695 0.000000 fcn <--> fcn      

##  Iterations =  0 



###########################################
#### Analyze Actigraph Data for Scott #####
####  1/11/2008                       #####
###########################################

library(foreign)
actigraph <- read.spss("./FinalData/Actigraph010808.sav",T,T)
act <- actigraph[,c(1,29:31)]
names(act)[1] <- c("personid")
kneeact <- merge(knee,act,by="personid")

kneeact$copingL <- zfa(kneeact[,104:110])$scores
kneeact$selfeL <- zfa(kneeact[,115:117])$scores
kneeact$fcnL <- zfa(kneeact[,c(111:114,120)])$scores

kneeact$copingI <- zfa(kneeact[,c(13,18,23,28,33,38,43)])$scores
kneeact$selfeI <- zfa(kneeact[,c(68,73,78)])$scores
kneeact$fcnI <- zfa(kneeact[,c(48,53,58,63,93)])$scores

act.l <- reshape(act,idvar="personid",varying=list(names(act)[2:4]),times=c(0,3,9),timevar="month",v.names=c("MVPA"),direction="long")

act.igc <- igc(act.l,"personid","month","MVPA")

pdf("MVPAigc.pdf")
plot(act.igc,main="MVPA Change",xlab="Months",ylab="MVPA")
dev.off()

act.out <- extractigc(act.igc,"MVPA.")

kneeact <- merge(kneeact,act.out,by="personid")

round(cor(kneeact[,132:137],kneeact[,c(129:131,138,139)],use="pairwise.complete.obs"),2)








#######################################
###### IGNORE CODE BELOW ##############
#######################################


#########################################################
################# OLD STUFF BELOW #######################
#########################################################


# Abstract 1 for Shelley Kassle

p1 <- merge(PANAS.scored,ADMIN,by.x="assessid",by.y="ASSESSID")[,c(3,4,5,16)]
m1 <- merge(MOS.scored,ADMIN,by.x="assessid",by.y="ASSESSID")[,c(3,4,15)]
w1 <- merge(WOMAC.scored,ADMIN,by.x="assessid",by.y="ASSESSID")[,c(3,4,5,6,17)]
v1 <- merge(VAS2.scored,ADMIN,by.x="assessid",by.y="ASSESSID")[,c(3,4,6,17)]

p1.wide <- reshape(p1,timevar="Time",idvar="personid",direction="wide")
m1.wide <- reshape(m1,timevar="Time",idvar="personid",direction="wide")
w1.wide <- reshape(w1,timevar="Time",idvar="personid",direction="wide")
v1.wide <- reshape(v1,timevar="Time",idvar="personid",direction="wide")

DEMO <- idparse(DEMO)[,c(3,28)]

a1.dat <- merge(p1.wide,m1.wide,by="personid")
a1.dat <- merge(a1.dat,w1.wide,by="personid")
a1.dat <- merge(a1.dat,v1.wide,by="personid")
a1.dat <- merge(a1.dat,DEMO,by="personid")
a1.dat <- merge(a1.dat,AGE,by.x="personid",by.y="PERSONID")

### Descriptive Statistics

summary(a1.dat)

#     personid      positive.0     negative.0      positive.3      negative.3   
#  Min.   :   1   Min.   :14.0   Min.   : 9.00   Min.   :13.00   Min.   : 9.00  
#  1st Qu.: 138   1st Qu.:29.0   1st Qu.:12.00   1st Qu.:32.25   1st Qu.:12.00  
#  Median : 350   Median :35.0   Median :14.00   Median :36.00   Median :14.00  
#  Mean   : 471   Mean   :34.4   Mean   :15.24   Mean   :35.05   Mean   :15.81  
#  3rd Qu.: 734   3rd Qu.:39.0   3rd Qu.:18.00   3rd Qu.:39.75   3rd Qu.:18.00  
#  Max.   :1241   Max.   :49.0   Max.   :35.00   Max.   :49.00   Max.   :33.00  
#                 NA's   : 5.0   NA's   : 4.00   NA's   :67.00   NA's   :65.00  
#    positive.9       negative.9        total.0         total.3      
#  Min.   : 19.00   Min.   :  9.00   Min.   :1.750   Min.   : 1.250  
#  1st Qu.: 33.00   1st Qu.: 12.00   1st Qu.:3.500   1st Qu.: 3.188  
#  Median : 36.00   Median : 14.00   Median :4.000   Median : 4.000  
#  Mean   : 36.14   Mean   : 15.18   Mean   :4.019   Mean   : 3.911  
#  3rd Qu.: 40.00   3rd Qu.: 17.00   3rd Qu.:5.000   3rd Qu.: 4.812  
#  Max.   : 50.00   Max.   : 32.00   Max.   :5.000   Max.   : 5.000  
#  NA's   :113.00   NA's   :113.00   NA's   :2.000   NA's   :65.000  
#     total.9            pain.0       stiffness.0     disability.0  
#  Min.   :  2.000   Min.   : 0.00   Min.   : 0.00   Min.   : 0.00  
#  1st Qu.:  3.500   1st Qu.: 6.20   1st Qu.: 4.00   1st Qu.: 7.53  
#  Median :  4.250   Median :13.70   Median :12.50   Median :14.62  
#  Mean   :  4.089   Mean   :17.10   Mean   :17.75   Mean   :20.06  
#  3rd Qu.:  4.750   3rd Qu.:23.85   3rd Qu.:27.00   3rd Qu.:30.07  
#  Max.   :  5.000   Max.   :64.80   Max.   :73.00   Max.   :84.88  
#  NA's   :113.000   NA's   : 1.00   NA's   : 1.00   NA's   : 7.00  
#      pain.3       stiffness.3     disability.3        pain.1      
#  Min.   : 0.00   Min.   : 0.00   Min.   : 0.000   Min.   :  0.80  
#  1st Qu.: 3.40   1st Qu.: 2.00   1st Qu.: 3.412   1st Qu.:  3.70  
#  Median : 9.80   Median : 8.50   Median : 9.647   Median :  7.40  
#  Mean   :14.79   Mean   :15.93   Mean   :15.610   Mean   : 11.75  
#  3rd Qu.:21.00   3rd Qu.:25.50   3rd Qu.:22.059   3rd Qu.: 14.10  
#  Max.   :72.20   Max.   :75.00   Max.   :68.588   Max.   : 64.40  
#  NA's   :64.00   NA's   :64.00   NA's   :64.000   NA's   :142.00  
#   stiffness.1      disability.1         pain.2        stiffness.2    
#  Min.   :  0.00   Min.   :  1.941   Min.   :  0.80   Min.   :  0.00  
#  1st Qu.:  2.25   1st Qu.:  4.559   1st Qu.:  2.50   1st Qu.:  2.00  
#  Median : 10.00   Median :  9.294   Median :  8.60   Median : 10.00  
#  Mean   : 12.39   Mean   : 13.267   Mean   : 13.23   Mean   : 16.95  
#  3rd Qu.: 19.00   3rd Qu.: 18.500   3rd Qu.: 24.20   3rd Qu.: 27.50  
#  Max.   : 61.50   Max.   : 45.941   Max.   : 37.00   Max.   : 61.00  
#  NA's   :142.00   NA's   :143.000   NA's   :146.00   NA's   :147.00  
#   disability.2          pain.6        stiffness.6      disability.6    
#  Min.   :  0.1765   Min.   :  2.20   Min.   :  0.00   Min.   :  1.765  
#  1st Qu.:  4.8824   1st Qu.:  7.85   1st Qu.:  1.25   1st Qu.:  5.397  
#  Median :  9.1765   Median : 11.60   Median :  6.25   Median :  7.382  
#  Mean   : 16.7507   Mean   : 14.18   Mean   : 11.12   Mean   : 10.775  
#  3rd Qu.: 30.2353   3rd Qu.: 15.20   3rd Qu.: 13.25   3rd Qu.: 11.015  
#  Max.   : 41.4118   Max.   : 46.40   Max.   : 51.50   Max.   : 44.765  
#  NA's   :148.0000   NA's   :157.00   NA's   :157.00   NA's   :157.000  
#      pain.9        stiffness.9      disability.9     KNEEPAINVAS.0  
#  Min.   :  0.00   Min.   :  0.00   Min.   :  0.000   Min.   : 0.00  
#  1st Qu.:  1.00   1st Qu.:  1.00   1st Qu.:  2.618   1st Qu.: 6.75  
#  Median :  5.10   Median :  4.25   Median :  6.765   Median :17.50  
#  Mean   : 12.01   Mean   : 11.23   Mean   : 13.098   Mean   :22.70  
#  3rd Qu.: 17.65   3rd Qu.: 18.25   3rd Qu.: 18.706   3rd Qu.:32.25  
#  Max.   : 69.40   Max.   : 47.50   Max.   : 61.647   Max.   :98.00  
#  NA's   :113.00   NA's   :113.00   NA's   :113.000   NA's   : 1.00  
#  ARTHRITISVAS.0  KNEEPAINVAS.3   ARTHRITISVAS.3  KNEEPAINVAS.9   
#  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00   Min.   :  0.00  
#  1st Qu.: 6.00   1st Qu.: 2.00   1st Qu.: 3.00   1st Qu.:  1.00  
#  Median :14.00   Median :10.00   Median :10.00   Median :  8.50  
#  Mean   :22.73   Mean   :18.44   Mean   :18.14   Mean   : 14.46  
#  3rd Qu.:33.00   3rd Qu.:26.00   3rd Qu.:28.00   3rd Qu.: 26.25  
#  Max.   :97.00   Max.   :86.00   Max.   :90.00   Max.   : 71.00  
#                  NA's   :64.00   NA's   :64.00   NA's   :113.00  
#  ARTHRITISVAS.9   SEX          AGE       
#  Min.   :  0.00    :  1   Min.   :35.00  
#  1st Qu.:  2.00   F:134   1st Qu.:47.00  
#  Median :  7.50   M: 34   Median :53.00  
#  Mean   : 17.38           Mean   :52.33  
#  3rd Qu.: 23.75           3rd Qu.:58.00  
#  Max.   : 93.00           Max.   :65.00  
#  NA's   :113.00                          

mean(a1.dat,na.rm=T)

#       personid     positive.0     negative.0     positive.3     negative.3 
#     470.988166      34.402439      15.236364      35.049020      15.807692 
#     positive.9     negative.9        total.0        total.3        total.9 
#      36.142857      15.178571       4.019461       3.911058       4.089286 
#         pain.0    stiffness.0   disability.0         pain.3    stiffness.3 
#      17.104762      17.752976      20.055919      14.792381      15.928571 
#   disability.3         pain.1    stiffness.1   disability.1         pain.2 
#      15.609524      11.748148      12.388889      13.266968      13.234783 
#    stiffness.2   disability.2         pain.6    stiffness.6   disability.6 
#      16.954545      16.750700      14.183333      11.125000      10.774510 
#         pain.9    stiffness.9   disability.9  KNEEPAINVAS.0 ARTHRITISVAS.0 
#      12.014286      11.232143      13.097689      22.696429      22.727811 
#  KNEEPAINVAS.3 ARTHRITISVAS.3  KNEEPAINVAS.9 ARTHRITISVAS.9            SEX 
#      18.438095      18.142857      14.464286      17.375000             NA 
#            AGE 
#      52.331361 

sd(a1.dat,na.rm=T)

#       personid     positive.0     negative.0     positive.3     negative.3 
#    377.7400213      7.0647483      4.6155766      6.8742272      5.4042000 
#     positive.9     negative.9        total.0        total.3        total.9 
#      6.8528125      4.9840004      0.8772536      1.0126136      0.8011761 
#         pain.0    stiffness.0   disability.0         pain.3    stiffness.3 
#     14.2791327     17.4812129     16.4887496     15.1452944     18.0789439 
#   disability.3         pain.1    stiffness.1   disability.1         pain.2 
#     15.2738164     13.9000379     13.1553830     11.1502011     11.9588160 
#    stiffness.2   disability.2         pain.6    stiffness.6   disability.6 
#     18.3873074     14.4628973     12.1060341     15.2302406     11.5133259 
#         pain.9    stiffness.9   disability.9  KNEEPAINVAS.0 ARTHRITISVAS.0 
#     14.7604315     13.8360413     14.9407439     21.4282702     22.3193114 
#  KNEEPAINVAS.3 ARTHRITISVAS.3  KNEEPAINVAS.9 ARTHRITISVAS.9            SEX 
#     20.6439243     20.3528625     16.5759566     21.2496524             NA 
#            AGE 
#      7.1553526 

# A bit more data preparation before we can test the mediation model

a1.dat$meanpain.0 <- rowMeans(a1.dat[,c(11,29)])
a1.dat$meanpain.3 <- rowMeans(a1.dat[,c(14,31)])

## Data analysis for the abstract

# Table 1

lm.table1 <- lm(positive.3~positive.0+total.0,data=a1.dat)

summary(lm.table1)

# Call:
# lm(formula = positive.3 ~ positive.0 + total.0, data = a1.dat)

# Residuals:
#       Min        1Q    Median        3Q       Max 
# -15.70675  -2.69755  -0.01163   2.40194  14.00980 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.96260    2.50448   3.579 0.000546 ***
# positive.0   0.61747    0.06425   9.611 1.12e-15 ***
# total.0      1.25838    0.53554   2.350 0.020856 *  
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 4.51 on 95 degrees of freedom
# Multiple R-Squared: 0.5772,	Adjusted R-squared: 0.5683 
# F-statistic: 64.84 on 2 and 95 DF,  p-value: < 2.2e-16 

## table 2

# step 1

lm.table2.1 <- lm(meanpain.3~meanpain.0,data=a1.dat)

summary(lm.table2.1)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -32.072  -8.025  -3.892   8.476  47.889 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.49959    2.22821   2.917  0.00436 ** 
# meanpain.0   0.52496    0.08816   5.954 3.82e-08 ***
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.3 on 101 degrees of freedom
# Multiple R-Squared: 0.2598,	Adjusted R-squared: 0.2525 
# F-statistic: 35.45 on 1 and 101 DF,  p-value: 3.815e-08 


lm.table2.2 <- lm(meanpain.3~meanpain.0+total.0,data=a1.dat)


summary(lm.table2.2)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0 + total.0, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -31.571  -7.945  -3.382   8.003  46.356 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.58077    6.35976   1.664   0.0994 .  
# meanpain.0   0.54270    0.08943   6.069 2.44e-08 ***
# total.0     -1.09739    1.55789  -0.704   0.4828    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.34 on 98 degrees of freedom
# Multiple R-Squared: 0.2731,	Adjusted R-squared: 0.2583 
# F-statistic: 18.41 on 2 and 98 DF,  p-value: 1.625e-07 


lm.table2.3 <- lm(meanpain.3~meanpain.0+total.0+positive.3,data=a1.dat)

summary(lm.table2.3)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0 + total.0 + positive.3, 
#     data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -31.293  -8.359  -2.786   7.763  46.392 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.04828    8.38346   1.079    0.283    
# meanpain.0   0.53803    0.09146   5.883 6.16e-08 ***
# total.0     -1.61953    1.76508  -0.918    0.361    
# positive.3   0.10912    0.23349   0.467    0.641    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.53 on 94 degrees of freedom
# Multiple R-Squared: 0.2715,	Adjusted R-squared: 0.2482 
# F-statistic: 11.67 on 3 and 94 DF,  p-value: 1.432e-06 


## table 3 - can be done by hand

## table 4 

lm.table4 <- lm(negative.3~negative.0+total.0,data=a1.dat)

summary(lm.table4)

# Call:
# lm(formula = negative.3 ~ negative.0 + total.0, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -6.2931 -2.4284 -0.5558  1.8423 15.6128 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.84117    2.42116   2.413   0.0177 *  
# negative.0   0.72647    0.08584   8.463 2.75e-13 ***
# total.0     -0.31228    0.45396  -0.688   0.4932    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 4.088 on 97 degrees of freedom
# Multiple R-Squared: 0.4398,	Adjusted R-squared: 0.4282 
# F-statistic: 38.07 on 2 and 97 DF,  p-value: 6.258e-13 


## table 5 steps 1 through 3

lm.table5.1 <- lm(meanpain.3~meanpain.0,data=a1.dat)

summary(lm.table5.1)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -32.072  -8.025  -3.892   8.476  47.889 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.49959    2.22821   2.917  0.00436 ** 
# meanpain.0   0.52496    0.08816   5.954 3.82e-08 ***
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.3 on 101 degrees of freedom
# Multiple R-Squared: 0.2598,	Adjusted R-squared: 0.2525 
# F-statistic: 35.45 on 1 and 101 DF,  p-value: 3.815e-08 


lm.table5.2 <- lm(meanpain.3~meanpain.0+total.0,data=a1.dat)

summary(lm.table5.2)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0 + total.0, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -31.571  -7.945  -3.382   8.003  46.356 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.58077    6.35976   1.664   0.0994 .  
# meanpain.0   0.54270    0.08943   6.069 2.44e-08 ***
# total.0     -1.09739    1.55789  -0.704   0.4828    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.34 on 98 degrees of freedom
# Multiple R-Squared: 0.2731,	Adjusted R-squared: 0.2583 
# F-statistic: 18.41 on 2 and 98 DF,  p-value: 1.625e-07 


lm.table5.3 <- lm(meanpain.3~meanpain.0+total.0+negative.3,data=a1.dat)

summary(lm.table5.3)

# Call:
# lm(formula = meanpain.3 ~ meanpain.0 + total.0 + negative.3, 
#     data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -29.292  -7.776  -2.485   6.835  47.939 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.55058    8.08522  -0.192   0.8483    
# meanpain.0   0.50878    0.08855   5.746 1.08e-07 ***
# total.0     -0.37272    1.55569  -0.240   0.8112    
# negative.3   0.62037    0.26519   2.339   0.0214 *  
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.03 on 96 degrees of freedom
# Multiple R-Squared: 0.3111,	Adjusted R-squared: 0.2895 
# F-statistic: 14.45 on 3 and 96 DF,  p-value: 7.677e-08 


## All means, parameter estimates, standard errors, and R-squares are
## reported above.

###################
### Abstract 2  ###
###################

a1.dat$womacfcn.0 <- rowMeans(a1.dat[,c(11,12,13)])
a1.dat$womacfcn.3 <- rowMeans(a1.dat[,c(14,15,16)])
a1.dat$womacfcn.9 <- rowMeans(a1.dat[,c(26,27,28)])

lm.ab2 <- lm(womacfcn.3~womacfcn.0+positive.0+negative.0 - 1,data=a1.dat)

summary(lm.ab2)

# Call:
# lm(formula = womacfcn.3 ~ womacfcn.0 + positive.0 + negative.0 - 
#     1, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -30.225  -6.835  -1.782   5.736  39.991 

# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# womacfcn.0  0.65337    0.08755   7.463 5.42e-11 ***
# positive.0  0.08569    0.08707   0.984    0.328    
# negative.0  0.04403    0.19430   0.227    0.821    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 12.04 on 89 degrees of freedom
# Multiple R-Squared: 0.7037,	Adjusted R-squared: 0.6937 
# F-statistic: 70.44 on 3 and 89 DF,  p-value: < 2.2e-16 

lm.ab2.1 <- lm(womacfcn.3~womacfcn.0+positive.3+negative.3 - 1,data=a1.dat)

summary(lm.ab2.1)

# Call:
# lm(formula = womacfcn.3 ~ womacfcn.0 + positive.3 + negative.3 - 
#     1, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -26.888  -6.614  -2.384   5.215  42.057 

# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# womacfcn.0  0.59944    0.08893   6.741 1.41e-09 ***
# positive.3 -0.01541    0.08315  -0.185   0.8534    
# negative.3  0.33827    0.17874   1.893   0.0616 .  
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 11.96 on 91 degrees of freedom
# Multiple R-Squared: 0.7011,	Adjusted R-squared: 0.6912 
# F-statistic: 71.14 on 3 and 91 DF,  p-value: < 2.2e-16 


lm.ab2.2 <- lm(pain.3~pain.0+positive.3+negative.3 - 1,data=a1.dat)

summary(lm.ab2.2)

# Call:
# lm(formula = pain.3 ~ pain.0 + positive.3 + negative.3 - 1, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -25.444  -6.205  -2.173   5.472  46.610 

# Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
# pain.0      0.55061    0.08621   6.387 5.85e-09 ***
# positive.3 -0.03590    0.08265  -0.434   0.6649    
# negative.3  0.43832    0.17285   2.536   0.0128 *  
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 12.19 on 97 degrees of freedom
# Multiple R-Squared: 0.6743,	Adjusted R-squared: 0.6642 
# F-statistic: 66.94 on 3 and 97 DF,  p-value: < 2.2e-16 


lm.ab2.3 <- lm(stiffness.3~stiffness.0+positive.3+negative.3 - 1,data=a1.dat)

summary(lm.ab2.3)


# Call:
# lm(formula = stiffness.3 ~ stiffness.0 + positive.3 + negative.3 - 
#     1, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -34.131  -7.325  -2.700   5.351  54.419 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# stiffness.0  0.57152    0.09189   6.220 1.26e-08 ***
# positive.3   0.03080    0.09611   0.320    0.749    
# negative.3   0.30597    0.20639   1.483    0.141    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 14.5 on 97 degrees of freedom
# Multiple R-Squared: 0.6163,	Adjusted R-squared: 0.6044 
# F-statistic: 51.93 on 3 and 97 DF,  p-value: < 2.2e-16 


lm.ab2.4 <- lm(disability.3~disability.0+positive.3+negative.3 - 1,data=a1.dat)

summary(lm.ab2.4)


# Call:
# lm(formula = disability.3 ~ disability.0 + positive.3 + negative.3 - 
#     1, data = a1.dat)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -29.486  -7.403  -3.302   5.164  44.281 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# disability.0  0.48706    0.08325   5.851 7.25e-08 ***
# positive.3    0.04594    0.08828   0.520    0.604    
# negative.3    0.29456    0.19142   1.539    0.127    
# ---
# Signif. codes:  0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 

# Residual standard error: 12.82 on 93 degrees of freedom
# Multiple R-Squared: 0.6691,	Adjusted R-squared: 0.6585 
# F-statistic:  62.7 on 3 and 93 DF,  p-value: < 2.2e-16 

pain.resid <- resid(lm(pain.3~pain.0,data=a1.dat))
lm.painneg <- lm(pain.resid~negative.3,data=a1.dat)


############################
## Pain analysis with Lee ##
############################

# Kneescreening dataset relevant for this analysis

# Need to change the factors into binary values

pain4months <- as.numeric(KNEESCREENING$PAIN4MONTHS) - 2
pain4months[pain4months == -1] <- NA

peakpainwk <- as.numeric(KNEESCREENING$PAINPERWEEKMOST) - 2
peakpainwk[peakpainwk == -1] <- NA

painbilat <- as.numeric(KNEESCREENING$PAINBOTHKNEES) - 2
painbilat[painbilat == -1] <- NA

painvas3 <- as.numeric(KNEESCREENING$PAINVAS3) - 2
painvas3[painvas3 == -1] <- NA

oadx <- as.numeric(KNEESCREENING$OADIAGNOSIS) - 2
oadx[oadx == -1] <- NA


raw.bayes <- function(x,priors=.5){
  # Notes:  x is a table specified as table(test (B), outcome (A))
  
  dat <- as.data.frame(x)

  # Basic signal detection computations
  
  tn <- dat[1,3] # TN
  fp <- dat[2,3] # FP
  fn <- dat[3,3] # FN
  tp <- dat[4,3] # TP
  sens <- tp/(tp+fn)
  spec <- tn/(tn+fp)
  pvp <- tp/(fp+tp)
  pvn <- tn/(tn+fn)
  tot <- sum(dat[,3])

  # Signal detection results

  sdt <- list(sensitivity=sens,specificity=spec,PVP=pvp,PVN=pvn)
  
  # Bayesian calculations
  # Let A = model goodness and B = Fit test

  # Good Models
  p.A.obs <- (fn+tp)/tot
  p.A.set <- priors # model priors
  p.notA.obs <- 1 - p.A.obs
  p.notA.set <- 1 - priors

  p.BgA <- tp/(tp+fn)
  p.BgnotA <- fp/(tn+fp)
  p.notBgA <- 1 - p.BgA # always based upon observed data
  p.notBgnotA <- 1 - p.BgnotA # likewise based upon observed data
  
  # Good Fit
  p.B.obs <- (fp+tp)/tot
  p.notB.obs <- 1 - p.B.obs
  p.B.set <- (p.BgA * p.A.set) + (p.BgnotA * p.notA.set)
  p.notB.set <- 1 - p.B.set

  ## the four posterior probabilities and their associated versions

  p.AgB.obs <- (p.BgA * p.A.obs)/ p.B.obs
  p.AgB.set <- (p.BgA * p.A.set) / p.B.set

  p.notAgB.obs <- (p.BgnotA * p.notA.obs) / p.B.obs
  p.notAgB.set <-  (p.BgnotA * p.notA.set) / p.B.set

  p.notAgnotB.obs <- (p.notBgnotA * p.notA.obs) / p.notB.obs
  p.notAgnotB.set <- (p.notBgnotA * p.notA.set) / p.notB.set

  p.AgnotB.obs <- (p.notBgA * p.A.obs) / p.notB.obs
  p.AgnotB.set <- (p.notBgA * p.A.set) / p.notB.set


  # results from bayesian analysis

  bayes <- list(compprior=p.A.obs,setprior=p.A.set,"P(A|B.obs)"=p.AgB.obs,"P(A|B.set)"=p.AgB.set,"P(~A|B.obs)"=p.notAgB.obs,"P(~A|B.set)"=p.notAgB.set,"P(~A|~B.obs)"=p.notAgnotB.obs,"P(~A|~B.set)"=p.notAgnotB.set,"P(A|~B.obs)"=p.AgnotB.obs,"P(A|~B.set)"=p.AgnotB.set)

  
  # Now some additional stats to return, OR, RR, Kappa, Overall
  # fraction correct, mis-classification rate, NNT, Absolute Risk
  # reduction, Relative risk reduction, Positive LR, Negative LR,
  # Diagnostic OR, Error OR, Youden's J, NND, Forbes NMI, contingency
  # coef, adjusted contingency coef, phi coef, Yule's Q.

  # SEE:  http://members.aol.com/johnp71/ctab2x2.html
  oddsratio <- (tp + fp)/(tn + fn)
  relrisk <- (tp/(tp+fp))/(fn/(fn + tn))
  kappa <- NA
  oafraccor <- (tp + tn)/tot
  missrate <- 1 - oafraccor
  NNT <- 1 / ((tp/(tp+fp)) - (fn/(fn + tn)))
  ARR <- (fn/(fn + tn)) - (tp/(tp+fp))
  RRR <- ARR/(fn/(fn + tn))
  PosLR <- sens/(1 - spec)
  NegLR <- (1 - sens) / spec
  DiagOR <- (sens/(1 - sens))/(spec/(1 - spec))
  YoudenJ <- sens + spec - 1
  NND <- 1/YoudenJ
  YulesQ <- (oddsratio - 1)/(oddsratio + 1)

  # Collect statistics for output and combine all three sets to display
  DxInds <- list(OR=oddsratio,RR=relrisk,Kappa=kappa,"Overall Fraction Correct"=oafraccor,"Miss Rate"=missrate,NNT=NNT,ARR=ARR,RRR=RRR, PosLR=PosLR, NegLR=NegLR, DiagOR=DiagOR, "Youden's J"=YoudenJ, NND=NND, "Yule's Q"=YulesQ)
  out <- list("Signal Detection Theory Results"=sdt,"Bayesian Results"=bayes,"Misc. Diagnostic Indicators"=DxInds)

  return(out)
}


Zfactor <- function(x){
  outdat <- data.frame(V1=rnorm(nrow(x)))
  for (i in 1:ncol(x)){
    outdat[,i] <- scale(x[,i])
  }
  outdat$sum <- rowMeans(outdat)
  itcor <- cor(x,outdat$sum,use="complete.obs")
  res <- list(outdat,"Item Total Correlation"=itcor)
  return(res)
}

raw.bayes(table(pain4months,oadx))
raw.bayes(table(peakpainwk,oadx))

pain.dat <- data.frame(pain4months,peakpainwk,painbilat,painvas3,oadx,painmonths=KNEESCREENING$PAINMONTHS,painperweek=KNEESCREENING$PAINPERWEEK,painvas=KNEESCREENING$PAINVAS,KNEESCREENING[,c(11:15)])

pain.dat$functionalprobs <- Zfactor(pain.dat[,c(8:13)])[[1]]$sum
pain.dat$binfactor <- Zfactor(pain.dat[,c(1:4,6)])[[1]]$sum

cor(pain.dat[,c(1:13,15)],pain.dat$functionalprobs,use="pairwise.complete.obs")
cor(pain.dat,use="pairwise.complete.obs")


########## Check to see which screening variables predict randomization ##########

screen.dat <- idparse(KNEESCREENING)
screen.dat$randomized <- 0
screen.dat$randomized[screen.dat$personid %in% DEMO$personid] <- 1

pain.dat$randomized <- screen.dat$randomized

raw.bayes(table(pain.dat$pain4months,pain.dat$randomized))
raw.bayes(table(pain.dat$peakpainwk,pain.dat$randomized))
raw.bayes(table(pain.dat$painbilat,pain.dat$randomized))
raw.bayes(table(pain.dat$painvas3,pain.dat$randomized))
raw.bayes(table(pain.dat$oadx,pain.dat$randomized))





################ SCRAP PILE ##################

table(ab1.dat$Time)

#   0   3   9 
# 175 107  56

table(ab1.dat$personid.y,ab1.dat$Time)
      
#        0 3 9
#   1    1 1 1
#   3    1 1 0
#   7    1 1 1
#   10   1 1 0
#   12   1 0 1
#   13   1 1 1
#   15   1 1 1
#   17   1 1 1
#   24   1 1 1
#   28   1 1 1
#   36   1 1 1
#   37   1 1 1
#   44   1 1 1
#   46   1 1 0
#   50   1 1 1
#   51   1 1 1
#   53   1 1 1
#   54   1 1 1
#   61   1 1 1
#   64   1 1 1
#   67   1 0 0
#   73   1 1 1
#   74   1 0 1
#   75   1 0 1
#   77   1 1 1
#   80   1 1 1
#   88   1 1 0
#   91   1 1 1
#   94   1 1 1
#   95   1 1 1
#   97   1 1 1
#   99   1 1 0
#   102  1 1 1
#   107  1 1 1
#   108  1 0 1
#   109  1 1 1
#   118  1 1 1
#   121  1 1 0
#   130  1 0 0
#   133  1 1 1
#   136  1 1 1
#   137  1 1 1
#   138  1 1 0
#   150  1 1 1
#   151  1 1 1
#   153  1 1 1
#   159  1 1 1
#   163  1 1 1
#   167  1 1 1
#   176  1 1 1
#   181  1 1 1
#   185  1 1 1
#   189  1 1 1
#   195  1 1 1
#   200  1 0 0
#   204  1 1 1
#   206  1 1 1
#   216  1 1 1
#   217  1 1 1
#   232  1 0 0
#   236  1 1 1
#   237  1 1 1
#   247  1 1 1
#   252  1 1 1
#   253  1 1 0
#   256  1 1 1
#   264  1 1 0
#   275  1 0 0
#   277  1 1 0
#   283  1 1 1
#   288  1 1 1
#   293  1 0 0
#   298  1 1 0
#   301  1 0 0
#   303  1 1 0
#   304  1 1 0
#   309  1 1 0
#   310  1 1 0
#   315  1 1 0
#   316  1 1 0
#   321  1 0 0
#   324  1 1 0
#   327  1 1 0
#   336  1 1 0
#   350  1 1 0
#   354  1 0 0
#   368  1 1 0
#   375  1 1 0
#   394  1 1 0
#   405  1 1 0
#   417  1 1 0
#   418  1 1 0
#   419  1 1 0
#   422  1 1 0
#   424  1 0 0
#   431  1 1 0
#   448  1 0 0
#   454  1 1 0
#   482  1 1 0
#   483  1 0 0
#   491  1 1 0
#   514  1 0 0
#   518  1 0 0
#   520  1 1 0
#   522  1 1 0
#   548  1 1 0
#   575  1 1 0
#   580  1 1 0
#   590  1 0 0
#   591  1 1 0
#   592  1 0 0
#   612  1 1 0
#   625  1 1 0
#   629  1 0 0
#   637  1 1 0
#   639  1 1 0
#   642  1 1 0
#   646  1 0 0
#   650  1 1 0
#   652  1 1 0
#   653  1 1 0
#   656  1 0 0
#   658  1 0 0
#   659  1 1 0
#   660  1 0 0
#   671  1 0 0
#   678  1 1 0
#   696  1 1 0
#   702  1 1 0
#   708  1 1 0
#   712  1 1 0
#   734  1 1 0
#   735  1 0 0
#   739  1 0 0
#   745  1 0 0
#   771  1 0 0
#   779  1 1 0
#   794  1 0 0
#   829  1 0 0
#   830  1 0 0
#   845  1 0 0
#   849  1 0 0
#   864  1 0 0
#   964  1 0 0
#   972  1 0 0
#   985  1 0 0
#   993  1 0 0
#   1023 1 0 0
#   1029 1 0 0
#   1035 1 0 0
#   1042 1 0 0
#   1049 1 0 0
#   1065 1 0 0
#   1073 1 0 0
#   1084 1 0 0
#   1089 1 0 0
#   1094 1 0 0
#   1095 1 0 0
#   1109 1 0 0
#   1110 1 0 0
#   1113 1 0 0
#   1122 1 0 0
#   1126 1 0 0
#   1135 1 0 0
#   1150 1 0 0
#   1158 1 0 0
#   1161 1 0 0
#   1172 1 0 0
#   1173 1 0 0
#   1193 1 0 0
#   1203 1 0 0
#   1204 1 0 0
#   1215 1 0 0
#   1232 1 0 0
#   1241 1 0 0

nrow(table(ab1.dat$personid.y,ab1.dat$Time))
# [1] 175  Sample size for the current data

mean(AGE$AGE)
# [1] 52.28889
sd(AGE$AGE)
# [1] 7.123405


## Reshape data into a wide format for later analysis

str(ab1.dat)

ab1.dat2 <- data.frame(ab1.dat[,c(2,29,30,31,55,56,65,71:84)])



############################################################
### Analyses 10/14/2007                                  ###
############################################################

# test model in parts

# list of datasets
# 1. ACLS - physical activity questionnaire
ACLS <- read.csv("./FinalData/ACLS.csv",T)

# 2. ADMIN -
ADMIN <- read.csv("./FinalData/ADMINASSESSMENT.csv",T)

# 3. AHI - arthritis helplessness inventory

# 4. CESD - measure of depression

# 5. CSQ - coping strategies questionnaire
CSQ <- read.csv("./FinalData/CSQ.txt",T)

# 6. EUROQOL - preference-based QoL measure
EUROQOL <- read.csv("./FinalData/EUROQOL.csv",T)

# 7. KLKNEEOA -
KLKNEEOA <- read.csv("./FinalData/KLKNEEOA.csv",T)

# 8. KNEESCREENING -
KNEESCREENING <- read.csv("./FinalData/KNEESCREENING.csv",T)

# 9. MOS - MOS Social Support Survey

# 10. NPI - NEO personality inventory

# 11. PANAS - positive and negative affect
PANAS <- read.csv("./FinalData/PANAS.csv",T)


# 12. PTASSESSMENT -

# 13. SELFE - Arthritis self-efficacy scale
SELFE <- read.csv("./FinalData/SELFE.csv",T)

# 14. SF36 - quality of life

# 15. STUDYSTATUS -
STUDYSTATUS <- read.csv("./FinalData/STUDYSTATUS.csv",T)


# 16. VAS2 - Pain and Arthritis severity visual analog scales

# 17. WOMAC - self-reported disability

# 18. 3RM
THREERM <- read.csv("./FinalData/3RM.csv",T)

# 19. CSARAW
CSARAW <- read.csv("./FinalData/CSARAW.csv",T)

# 20. AE
AE <- read.csv("./FinalData/AE.csv",T)

# 21. AGE
AGE <- read.csv("./FinalData/AGE.csv",T)


EXERCISE <- read.csv("./FinalData/EXERCISE.csv",T)

PEDOMETER <- read.csv("./FinalData/PEDOMETER.csv",T)
SELFMANSESSQA <- read.csv("./FinalData/SELFMANSESSQA.csv",T)
VAS2 <- read.csv("./FinalData/VAS2.csv",T)
AHI <- read.csv("./FinalData/AHI.csv",T)
DEMO <- read.csv("./FinalData/DEMO.csv",T)
EXRECAP <- read.csv("./FinalData/EXRECAP.csv",T)
MOS <- read.csv("./FinalData/MOS.csv",T)
PERSON <- read.csv("./FinalData/PERSON.csv",T)
SELFMANSUMMARY <- read.csv("./FinalData/SELFMANSUMMARY.csv",T)
WOMAC <- read.csv("./FinalData/WOMAC.csv",T)
ADMIN <- read.csv("./FinalData/ADMIN.csv",T)
CESD <- read.csv("./FinalData/CESD.csv",T)
ECON <- read.csv("./FinalData/ECON.csv",T)
FUNCTTEST <- read.csv("./FinalData/FUNCTTEST.csv",T)
NPI <- read.csv("./FinalData/NPI.csv",T)
PTASSESSMENT <- read.csv("./FinalData/PTASSESSMENT.csv",T)
SF36 <- read.csv("./FinalData/SF36.csv",T)
