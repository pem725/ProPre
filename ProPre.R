######################################################################
######################################################################
### Analysis for promotion/prevention paper with Dan and Alex
###
### Latest Revision:  9/16/13
###
### Authors: Patrick E. McKnight and Dan Blalock
###
### Author: Patrick E. McKnight, Ph.D.
###         George Mason University
###         Department of Psychology
###         pmcknigh@gmu.edu
###
###
######################################################################
######################################################################

# Include section ------------------------

## Script for Promotion/Prevention Composite Variables ##
source("./zfa.R")
source("./igc.R")
#library(devtools)
#install_github('MRES',"pem725")

## Whatever
#install.packages(c("car","mice","psy","lme4","reshape","sem"))
library(car)
library(mice)
library(psy)
library(lme4)
library(reshape)
library(sem)

idparse <- function(x){
  x$study <- as.integer(substr(as.character(x$ASSESSID),1,2))
  x$personid <- as.integer(substr(as.character(x$ASSESSID),3,6))
  x$assessid <- as.integer(substr(as.character(x$ASSESSID),7,9))
  return(x)
}

assessidparse <- function(x){
  x$month <- recode(x$assessid,"501=0;520=0;521=3;540=9;522=18;541=24;530=1;531=2;532=6;533=12;534=15;535=21;560=0;561=9;562=24;570=0;571=3;572=9;573=18;574=24",F)
  return(x)
}

# Data handling ---------------------

knee.l <- read.csv("./KNEEnotreduced.csv")
knee <- read.csv("./KNEEreduced.csv")


NPI <- read.csv(file="NPI.csv",head=TRUE,sep=",")
AHI <- read.csv(file="AHI.csv",head=TRUE,sep=",")
PANAS <- read.csv(file="PANAS.csv",head=TRUE,sep=",")
SF36 <- read.csv(file="SF36.csv",head=TRUE,sep=",")
CESD <- read.csv(file="CESD.csv",head=TRUE,sep=",")

## NPI
# +1 is to account for assessid var at beginning

NPIff <- assessidparse(idparse(NPI))


NPIffr <- NPIff
ritems.NPI <- c(8,15,21,25,27,28,35,37)
for (i in 1:length(ritems.NPI)){
  NPIffr[,ritems.NPI[i] + 1] <- abs(NPIffr[,ritems.NPI[i]+1] - 6)
}

# Additional NPI Recoding According to new Composites

ritems2.NPI <- c(20,4,17,21,27)
for (i in 1:length(ritems.NPI)){
  NPIffr[,ritems.NPI[i] + 1] <- abs(NPIffr[,ritems.NPI[i]+1] - 6)
}

## delete useless variables from data.frame
NPIffr <- NPIffr[,-c(1,39,41)]

## AHI
# +1 is to account for assessid var at beginning

AHIff <- assessidparse(idparse(AHI))

recodeAHI <- function(x){
  ritems <- c(2,3,5,6,8,9,11,13,15)
  for (i in 1:length(ritems)){
    x[,ritems[i]+1] <- recode(x[,ritems[i] + 1],"1=4;2=3;3=2;4=1",F)
  }
  return(x)
}

AHIffr <- recodeAHI(AHIff)

# Additional AHI Recoding According to new Composites

recode2AHI <- function(x){
  ritems <- c(2,5,6,8,9,14,15)
  for (i in 1:length(ritems)){
    x[,ritems[i]+1] <- recode(x[,ritems[i] + 1],"1=4;2=3;3=2;4=1",F)
  }
  return(x)
}

AHIffr <- recodeAHI(AHIff)
AHIffr <- recode2AHI(AHIffr)
AHIffr <- AHIffr[,-c(1,17,19)]

## PANAS
# Positive Affect PANASPOS        1,3,5,9,10,12,14,16,17,19
# Negative Affect PANASNEG        2,4,6,7,8,11,13,15,18,20

PANASffr <- assessidparse(idparse(PANAS))
PANASffr <- PANASffr[,-c(1,22,24)]


## SF36

SF36ff <- complete(mice(SF36[,-1]),1)
SF36ffr <- data.frame(ASSESSID=SF36[,1],SF36ff)
SF36ffr <- assessidparse(idparse(SF36ffr))

# Additional SF36 Recoding According to new Composites
names(SF36ffr)
recodeSF36ffr <- function(x){
  ritems <- c(26,29,30,32,34)
  for (i in 1:length(ritems)){
    x[,ritems[i]+1] <- recode(x[,ritems[i] + 1],"1=6;2=5;3=4;4=3;5=2;6=1",F)
  }
  return(x)
}

SF36ffr <- recodeSF36ffr(SF36ffr)
SF36ffr <- SF36ffr[,-c(1,38,40)]


## CESD
# Reverse-Coded CESD Items 4,8,12,16

#CESDr <- CESD
#CESDr[,5] <- recode(CESDr[,5],"0=3;1=2;2=1;3=0",F)
#CESDr[,9] <- recode(CESDr[,9],"0=3;1=2;2=1;3=0",F)
#CESDr[,13] <- recode(CESDr[,13],"0=3;1=2;2=1;3=0",F)
#CESDr[,17] <- recode(CESDr[,17],"0=3;1=2;2=1;3=0",F)

# Additional CESD Recoding According to new Composites
CESDr <- CESD

recodeCESDr <- function(x){
  ritems <- c(3,4,5,6,9,20)+1
  for (i in 1:length(ritems)){
    x[,ritems[i]] <- recode(x[,ritems[i]],"0=3;1=2;2=1;3=0",F)
  }
  return(x)
}

CESDffr <- recodeCESDr(CESDr)
CESDffr <- assessidparse(idparse(CESDr))
CESDffr <- complete(mice(CESDffr[,-1]),1)
CESDffr <- CESDffr[,-c(21,23)]

## Pro/Pre Dataset

PPffr <- merge(NPIffr,AHIffr,all=T,by=c("personid","month"))
PPffr <- merge(PPffr,PANASffr,all=T,by=c("personid","month"))
PPffr <- merge(PPffr,SF36ffr,all=T,by=c("personid","month"))
PPffr <- merge(PPffr,CESDffr,all=T,by=c("personid","month"))
                         
## Individual Scale Reliabilities

##### CHANGE THE COLUMN NUMBERS AFTER WE DROPPED THE REDUNDANT VARIABLES
## NPIffralpha = cronbach(PPffr[, 4:40])
## AHIffralpha = cronbach(PPffr[, 44:58])
## PANASffralpha = cronbach(PPffr[, 62:81])
## SF36ffralpha = cronbach(PPffr[, 85:120])
## CESDffralpha = cronbach(PPffr[, 123:142])

## Promotion/Prevention Composites
# SF-36 Removed from Composites.  Questions ask about state-level functioning. Composites represent trait-level variables.
# Also, DV is concerned with functioning - so SF-36 would confound analysis.
# Promotion SF-36: "SF3609A","SF3609C","SF3609E","SF3609F","SF3609G","SF3609H","SF3609I","SF3611A"
# Prevention SF-36: "SF3609B","SF3611C"

Promotion <- PPffr[c( "personid","month","NPI01","NPI03","NPI06","NPI16","NPI19","NPI33","AHI02","AHI05","AHI06","AHI08","AHI09","AHI15","PANAS01","PANAS03","PANAS05","PANAS09","PANAS10","PANAS12","PANAS14","PANAS16","PANAS17","PANAS19","CESD04","CESD08","CESD12","CESD16")] ## removed items CESD06 CESD09

Prevention <- PPffr[c("personid","month","NPI10","NPI18","NPI21","NPI22","NPI27","AHI04","AHI07","AHI12","PANAS02","PANAS04","PANAS06","PANAS07","PANAS08","PANAS11","PANAS13","PANAS15","PANAS18","PANAS20","CESD05","CESD10","CESD11")]

## REMOVED AHI13 from Prevention and AHI14 from Promotion because they
## were reverse coded from the beginning - we wanted to restrict our
## analysis and measurement model to only those items that were
## positively worded.

## create raw score data.frame for full SEM model
PPraw <- merge(Promotion,Prevention,by=c("personid","month"))


## Promotion/Prevention Composite Reliabilities
# Taking out SF-36 items DRAMATICALLY improves alphas to acceptable levels.
#ProAlpha = cronbach(Promotion[,-c1])
#PreAlpha = cronbach(Prevention[,-1])

## Promotion & Prevention Composite Item Correlations

cor(Promotion[,-c(1,2)], use="pairwise.complete.obs",method="pearson")
# Items below were inconsistent, reviewed, and removed - all were negatively worded
# NPI20: "I am not a cheerful optimist" Negatively worded
# CESD03: "I felt that I could not shake off the blues even with help from my family or friends" Negatively worded
# CESD20: "I could not get 'going'" Negatively worded
cor(Prevention[,-c(1,2)], use="pairwise.complete.obs",method="pearson")
# Items below were inconsistent, reviewed, and removed - all were negatively worded
# NPI04: "I am not a worrier" Negatively worded
# NPI15: "I rarely feel fearful or anxious" Negatively worded
# NPI17: "I am seldom sad or depressed" Negatively worded

Pro.zfa <- zfa(Promotion[,-c(1,2)])
Pre.zfa <- zfa(Prevention[,-c(1,2)])
summary(Pro.zfa)
summary(Pre.zfa)


length(Pro.zfa$scores)
length(Pre.zfa$scores)
PP.dat <- data.frame(personid=Promotion$personid,month=Promotion$month,Pro=Pro.zfa$scores,Pre=Pre.zfa$scores)

cor(PP.dat$Pro,PP.dat$Pre)

lm.Pro <- lm(Pro~as.factor(month),data=PP.dat)
lm.Pre <- lm(Pre~as.factor(month),data=PP.dat)

summary(lm.Pro)
summary(lm.Pre)

Pro.agg <- aggregate(PP.dat$Pro,by=list(PP.dat$personid),mean)
Pre.agg <- aggregate(PP.dat$Pre,by=list(PP.dat$personid),mean)

names(Pro.agg) <- c("personid","Pro")
names(Pre.agg) <- c("personid","Pre")

PPfin <- merge(Pro.agg,Pre.agg,by="personid")
cor(PPfin$Pro,PPfin$Pre)

## raw manifest variables included in new dataset
PPraw.agg <- aggregate(PPraw[,-c(1:2)],by=list(PPraw$personid),mean,na.rm=T)
names(PPraw.agg)[1] <- "personid"


## Now create compliance data

excomp <- read.csv("./EXcompliance.csv")
smcomp <- read.csv("./SMcompliance.csv")
smcomp.sum <- aggregate(smcomp$attend,by=list(smcomp$PERSONID),mean,na.rm=T)
names(smcomp.sum) <- c("personid","smcomp")

excomp$excomp <- rowMeans(excomp[,2:5],na.rm=T)
compliance <- merge(excomp,smcomp.sum,by="personid",all.x=T,all.y=T)
compliance$comp <- rowMeans(compliance[,6:7],na.rm=T)

PPdatF <- merge(knee,PPfin,by="personid")
PPdatF <- merge(PPdatF,compliance,by="personid")

## raw manifest variables now added (see above)
PPdatFraw <- merge(knee,PPraw.agg,by="personid")
PPdatFraw <- merge(PPdatFraw,compliance,by="personid")

## NOTE: had to change the index numbers when I dropped the AHI variables
PPdatFraw$NPI.pro <- zfa(PPdatFraw[,124:129])$scores
PPdatFraw$AHI.pro <- zfa(PPdatFraw[,130:135])$scores
PPdatFraw$PANAS.pro <- zfa(PPdatFraw[,136:145])$scores
PPdatFraw$CESD.pro <- zfa(PPdatFraw[,146:149])$scores

PPdatFraw$NPI.pre <- zfa(PPdatFraw[,150:154])$scores
PPdatFraw$AHI.pre <- zfa(PPdatFraw[,155:157])$scores
PPdatFraw$PANAS.pre <- zfa(PPdatFraw[,158:167])$scores
PPdatFraw$CESD.pre <- zfa(PPdatFraw[,168:170])$scores

# Data Analyses -------------------

## 1.  Are promotion and prevention related to treatment?
library(lme4)
pro.lme <- lmer(Pro~GLabel+(1 | GLabel),data=PPdatF[!is.na(PPdatF$GLabel),])
pre.lme <- lmer(Pre~GLabel+(1 | GLabel),data=PPdatF[!is.na(PPdatF$GLabel),])

summary(lm(pro.lme))
summary(lm(pre.lme))

## ANS: Yes, the groups differ with respect to both risk and
## resilience.

## 2.  Does the following model fit across all subjects?

indiff.mod1 <- matrix(c('Pre -> comp','b1',NA,
                        'Pre -> SE','b2',NA,
                        'Pro -> SE','b3',NA,
                        'Pre -> Func','b4',NA,
                        'Pro -> Func','b5',NA,
                        'comp -> SE','b6',NA,
                        'comp -> Func','b7',NA,
                        'SE -> Func','b8',NA,
                        'Pre <-> Pro','rrCOV',NA,
                        'Pre <-> Pre','th1',NA,
                        'Pro <-> Pro','th2',NA,
                        'comp <-> comp','th3',NA,
                        'SE <-> SE','th4',NA,
                        'Func <-> Func','th5',NA),ncol=3,byrow=T)

indiff.mod2 <- matrix(c('Pro -> comp','b1',NA,
                        'Pre -> SE','b2',NA,
                        'Pro -> SE','b3',NA,
                        'Pre -> Func','b4',NA,
                        'Pro -> Func','b5',NA,
                        'comp -> SE','b6',NA,
                        'comp -> Func','b7',NA,
                        'SE -> Func','b8',NA,
                        'Pre <-> Pro','rrCOV',NA,
                        'Pre <-> Pre','th1',NA,
                        'Pro <-> Pro','th2',NA,
                        'comp <-> comp','th3',NA,
                        'SE <-> SE','th4',NA,
                        'Func <-> Func','th5',NA),ncol=3,byrow=T)


#                        'risk -> comp','b1',NA, ## ELIMINATED DUE TO OVERFITTING

pp.mod.raw <- matrix(c('Pro -> NPI.pro','lam11',NA,
                       'Pro -> AHI.pro','lam12',NA,
                       'Pro -> PANAS.pro','lam13',NA,
                       'Pro -> CESD.pro','lam14',NA,
                       'Pre -> NPI.pre','lam21',NA,
                       'Pre -> AHI.pre','lam22',NA,
                       'Pre -> PANAS.pre','lam23',NA,
                       'Pre -> CESD.pre','lam24',NA,
                       'NPI.pro <-> NPI.pro','th11',NA,
                       'AHI.pro <-> AHI.pro','th12',NA,
                       'PANAS.pro <-> PANAS.pro','th13',NA,
                       'CESD.pro <-> CESD.pro','th14',NA,
                       'NPI.pre <-> NPI.pre','th21',NA,
                       'AHI.pre <-> AHI.pre','th22',NA,
                       'PANAS.pre <-> PANAS.pre','th23',NA,
                       'CESD.pre <-> CESD.pre','th24',NA,
                       'NPI.pro <-> NPI.pre','gam1',NA,
                       'AHI.pro <-> AHI.pre','gam2',NA,
                       'PANAS.pro <-> PANAS.pre','gam3',NA,
                       'CESD.pro <-> CESD.pre','gam4',NA,
                       'Pro -> comp','b1',NA,
                       'Pre -> comp','b2',NA,
                       'Pre -> SE','b3',NA,
                       'Pro -> SE','b4',NA,
                        'Pre -> Func','b5',NA,
                        'Pro -> Func','b6',NA,
                        'comp -> SE','b7',NA,
                        'comp -> Func','b8',NA,
                        'SE -> Func','b9',NA,
                        'Pre <-> Pro','rrCOV',NA,
                        'Pre <-> Pre',NA,1,
                        'Pro <-> Pro',NA,1,
                        'comp <-> comp','th3',NA,
                        'SE <-> SE','th4',NA,
                        'Func <-> Func','th5',NA),ncol=3,byrow=T)


## Remove AHI
## pp.mod2.raw <- matrix(c('Pro -> NPI.pro','lam11',NA,
##                        'Pro -> PANAS.pro','lam13',NA,
##                        'Pro -> CESD.pro','lam14',NA,
##                        'Pre -> NPI.pre','lam21',NA,
##                        'Pre -> PANAS.pre','lam23',NA,
##                        'Pre -> CESD.pre','lam24',NA,
##                        'NPI.pro <-> NPI.pro','th11',NA,
##                        'PANAS.pro <-> PANAS.pro','th13',NA,
##                        'CESD.pro <-> CESD.pro','th14',NA,
##                        'NPI.pre <-> NPI.pre','th21',NA,
##                        'PANAS.pre <-> PANAS.pre','th23',NA,
##                        'CESD.pre <-> CESD.pre','th24',NA,
##                        'NPI.pro <-> NPI.pre','gam1',NA,
##                        'PANAS.pro <-> PANAS.pre','gam3',NA,
##                        'CESD.pro <-> CESD.pre','gam4',NA,
##                        'Pro -> comp','b1',NA,
##                        'Pre -> comp','b2',NA,
##                        'Pre -> SE','b3',NA,
##                        'Pro -> SE','b4',NA,
##                         'Pre -> Func','b5',NA,
##                         'Pro -> Func','b6',NA,
##                         'comp -> SE','b7',NA,
##                         'comp -> Func','b8',NA,
##                         'SE -> Func','b9',NA,
##                         'Pre <-> Pro','rrCOV',NA,
##                         'Pre <-> Pre',NA,1,
##                         'Pro <-> Pro',NA,1,
##                         'comp <-> comp','th3',NA,                        
##                         'SE <-> SE','th4',NA,
##                         'Func <-> Func','th5',NA),ncol=3,byrow=T)


i.dat <- PPdatF[,c("Pre","Pro","comp","sepainRes","sefcnRes","seosxRes","sf36totRes","sf36pfRes","sf36bpRes","sf36phRes")]

se.zfa <- zfa(i.dat[,4:6])
func.zfa <- zfa(i.dat[,7:10])

i.dat <- cbind(i.dat[,1:3],se.zfa$scores,func.zfa$scores)

names(i.dat) <- c("Pre","Pro","comp","SE","Func")

## raw data file
i2.dat <- cbind(PPdatFraw[,c(178:185,177)],zfa(PPdatFraw[,116:118])$scores,zfa(PPdatFraw[,112:115])$scores)
names(i2.dat)[10:11] <- c("SE","Func")

## impute values for i.dat

library(mice)
i.mice <- mice(i.dat)

i.cov1 <- cov(complete(i.mice,1))
i.cov2 <- cov(complete(i.mice,2))
i.cov3 <- cov(complete(i.mice,3))
i.cov4 <- cov(complete(i.mice,4))
i.cov5 <- cov(complete(i.mice,5))

## run models above with sem for each complete dataset:

library(sem)
i.sem.1 <- sem(indiff.mod1,i.cov1,250)
i.sem.2 <- sem(indiff.mod1,i.cov2,250)
i.sem.3 <- sem(indiff.mod1,i.cov3,250)
i.sem.4 <- sem(indiff.mod1,i.cov4,250)
i.sem.5 <- sem(indiff.mod1,i.cov5,250)

summary(i.sem.1)

##  Model Chisquare =  0.14391   Df =  1 Pr(>Chisq) = 0.70442
##  Chisquare (null model) =  219.52   Df =  10
##  Goodness-of-fit index =  0.99977
##  Adjusted goodness-of-fit index =  0.99653
##  RMSEA index =  0   90% CI: (NA, 0.12233)
##  Bentler-Bonnett NFI =  0.99934
##  Tucker-Lewis NNFI =  1.0409
##  Bentler CFI =  1
##  SRMR =  0.0054955
##  AIC =  28.144
##  AICc =  1.9311
##  BIC =  77.444
##  CAIC =  -6.3776

##  Normalized Residuals
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.3100 -0.0343 -0.0140 -0.0451  0.0000  0.0000 

##  R-square for Endogenous Variables
##   comp     SE   Func 
## 0.0175 0.1622 0.2676 

##  Parameter Estimates
##       Estimate  Std Error z value  Pr(>|z|)                 
## b1    -0.064686 0.0306723 -2.10894 3.4950e-02 comp <--- Pre 
## b2    -0.336841 0.1033071 -3.26058 1.1118e-03 SE <--- Pre   
## b3     0.153532 0.0951119  1.61422 1.0648e-01 SE <--- Pro   
## b4     0.039913 0.1106540  0.36070 7.1832e-01 Func <--- Pre 
## b5     0.345833 0.1002890  3.44836 5.6400e-04 Func <--- Pro 
## b6     0.672079 0.1756834  3.82551 1.3050e-04 SE <--- comp  
## b7     0.075616 0.1896227  0.39877 6.9006e-01 Func <--- comp
## b8     0.466966 0.0664750  7.02468 2.1455e-12 Func <--- SE  
## rrCOV -0.195985 0.0254001 -7.71590 1.2013e-14 Pro <--> Pre  
## th1    0.323854 0.0290245 11.15796 6.5479e-29 Pre <--> Pre  
## th2    0.377444 0.0338273 11.15796 6.5479e-29 Pro <--> Pro  
## th3    0.075865 0.0067992 11.15796 6.5479e-29 comp <--> comp
## th4    0.583043 0.0522536 11.15796 6.5479e-29 SE <--> SE    
## th5    0.641530 0.0574953 11.15796 6.5479e-29 Func <--> Func

##  Iterations =  0 

#path.diagram(i.sem.1,out.file="./RRsem.dot",edge.labels="values")
pathDiagram(i.sem.1,file="PPsem",edge.labels="both",standardize=T)

## hand edited to created dashed lines for ns paths
system("dot2tex -f pst RRsem.dot > RRsem.tex")

## what does this mean with respect to NNT
datNNT <- complete(i.mice)
datNNT$Pro.bin <- "Low"
datNNT$Pro.bin[datNNT$Pro >= median(datNNT$Pro)] <- "High"
datNNT$Pro.bin <- as.factor(datNNT$Pro.bin)
datNNT$Pre.bin <- "Low"
datNNT$Pre.bin[datNNT$Pre >= median(datNNT$Pre)] <- "High"
datNNT$Pre.bin <- as.factor(datNNT$Pre.bin)


datNNT.agg <- aggregate(datNNT[,3:5],by=list(datNNT$Pro.bin,datNNT$Pre.bin),mean)
datNNT.agg

## A bivariate median split on Pro and Pre produces the following finding...

##   Group.1 Group.2      comp          SE        Func
## 1     Low     Low 0.5753383 -0.25127658 -0.28585112
## 2    High     Low 0.6522881  0.05885877  0.09670165
## 3     Low    High 0.6427537 -0.60857374 -0.52234519
## 4    High    High 0.1964212 -0.61137677  0.07180806

## OR...

##        comp         SE         Func
## PRO  0.6522881  0.05885877  0.09670165
## PRE  0.6427537 -0.60857374 -0.52234519

## Summary: Promotion oriented folks did not change a whole lot with
## respect to SE (0.06) or Func (0.10) but Prevention oriented volks
## got a whole lot worse.  Note that neither group had different
## compliance rates.

## check it out in a simple lm
lmComp <- lm(comp~Pro+Pre,data=datNNT)
summary(lmComp)
plot(lmComp)

lmComp2 <- lm(comp~Pro+Pre,data=datNNT[-c(92,151,163),])
summary(lmComp2)
plot(lmComp2)


## Try promotion predicting compliance and omit prevention as direct effect

i.sem.1a <- sem(indiff.mod2,i.cov1,250)
## i.sem.2 <- sem(indiff.mod1,i.cov2,250)
## i.sem.3 <- sem(indiff.mod1,i.cov3,250)
## i.sem.4 <- sem(indiff.mod1,i.cov4,250)
## i.sem.5 <- sem(indiff.mod1,i.cov5,250)

summary(i.sem.1a) ## no significant difference between models:

##  Model Chisquare =  3.709   Df =  1 Pr(>Chisq) = 0.054121
##  Chisquare (null model) =  213.1   Df =  10
##  Goodness-of-fit index =  0.99412
##  Adjusted goodness-of-fit index =  0.91181
##  RMSEA index =  0.1043   90% CI: (NA, 0.22629)
##  Bentler-Bonnett NFI =  0.98259
##  Tucker-Lewis NNFI =  0.86662
##  Bentler CFI =  0.98666
##  SRMR =  0.026742
##  AIC =  31.709
##  AICc =  5.4962
##  BIC =  81.009
##  CAIC =  -2.8125

##  Normalized Residuals
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -1.57000  0.00000  0.00000 -0.11900  0.00979  0.35300 

##  R-square for Endogenous Variables
##   comp     SE   Func 
## 0.0036 0.1213 0.2596 

##  Parameter Estimates
##       Estimate  Std Error z value  Pr(>|z|)                 
## b1     0.027230 0.0287374  0.94756 3.4335e-01 comp <--- Pro 
## b2    -0.326480 0.1038063 -3.14509 1.6604e-03 SE <--- Pre   
## b3     0.146085 0.0955400  1.52904 1.2625e-01 SE <--- Pro   
## b4     0.149601 0.1120058  1.33566 1.8166e-01 Func <--- Pre 
## b5     0.380486 0.1015717  3.74598 1.7969e-04 Func <--- Pro 
## b6     0.441621 0.1722450  2.56391 1.0350e-02 SE <--- comp  
## b7     0.057306 0.1846557  0.31034 7.5630e-01 Func <--- comp
## b8     0.489913 0.0670592  7.30568 2.7586e-13 Func <--- SE  
## rrCOV -0.197596 0.0251385 -7.86031 3.8319e-15 Pro <--> Pre  
## th1    0.316190 0.0283376 11.15796 6.5479e-29 Pre <--> Pre  
## th2    0.374173 0.0335342 11.15796 6.5479e-29 Pro <--> Pro  
## th3    0.076942 0.0068957 11.15796 6.5479e-29 comp <--> comp
## th4    0.568405 0.0509417 11.15796 6.5479e-29 SE <--> SE    
## th5    0.636464 0.0570412 11.15796 6.5479e-29 Func <--> Func

##  Iterations =  0 


### RAW DATA MODEL - NOT TO BE REPORTED BUT WE NEED TO UNDERSTAND WHY THE PARAMS change

i2.mice <- mice(i2.dat)

#sem.r1 <- sem(pp.mod.raw,cov(complete(i2.mice)),250)
#summary(sem.r1)


#summary(sem.r1)

##  Model Chisquare =  89.095   Df =  33 Pr(>Chisq) = 4.7132e-07
##  Chisquare (null model) =  873.96   Df =  55
##  Goodness-of-fit index =  0.93968
##  Adjusted goodness-of-fit index =  0.87936
##  RMSEA index =  0.082624   90% CI: (0.062222, 0.10351)
##  Bentler-Bonnett NFI =  0.89806
##  Tucker-Lewis NNFI =  0.88584
##  Bentler CFI =  0.9315
##  SRMR =  0.053487
##  AIC =  155.1
##  AICc =  99.484
##  BIC =  271.3
##  CAIC =  -126.11

##  Normalized Residuals
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -1.80000 -0.49600 -0.00001  0.02570  0.76000  2.79000 

##  R-square for Endogenous Variables
##   NPI.pro   AHI.pro PANAS.pro  CESD.pro   NPI.pre   AHI.pre PANAS.pre  CESD.pre 
##    0.3245    0.0902    0.7385    0.6643    0.5561    0.0450    0.6330    0.4540 
##      comp        SE      Func 
##    0.0322    0.2530    0.2518 

##  Parameter Estimates
##       Estimate  Std Error z value   Pr(>|z|)                           
## lam11  0.386798 0.0418884   9.23400 2.6072e-20 NPI.pro <--- Pro        
## lam12  0.178862 0.0391634   4.56706 4.9460e-06 AHI.pro <--- Pro        
## lam13  0.705740 0.0461656  15.28714 9.3153e-53 PANAS.pro <--- Pro      
## lam14  0.687331 0.0482381  14.24871 4.5662e-46 CESD.pro <--- Pro       
## lam21  0.491898 0.0385094  12.77347 2.3065e-37 NPI.pre <--- Pre        
## lam22  0.142087 0.0452324   3.14126 1.6822e-03 AHI.pre <--- Pre        
## lam23  0.625452 0.0458576  13.63901 2.3473e-42 PANAS.pre <--- Pre      
## lam24  0.470741 0.0426593  11.03488 2.5940e-28 CESD.pre <--- Pre       
## th11   0.311387 0.0301549  10.32627 5.3604e-25 NPI.pro <--> NPI.pro    
## th12   0.322491 0.0293394  10.99171 4.1890e-28 AHI.pro <--> AHI.pro    
## th13   0.176351 0.0329201   5.35694 8.4642e-08 PANAS.pro <--> PANAS.pro
## th14   0.238776 0.0351589   6.79134 1.1110e-11 CESD.pro <--> CESD.pro  
## th21   0.193142 0.0230572   8.37665 5.4454e-17 NPI.pre <--> NPI.pre    
## th22   0.428114 0.0387030  11.06152 1.9281e-28 AHI.pre <--> AHI.pre    
## th23   0.226829 0.0323529   7.01111 2.3644e-12 PANAS.pre <--> PANAS.pre
## th24   0.266488 0.0289878   9.19313 3.8159e-20 CESD.pre <--> CESD.pre  
## gam1  -0.064997 0.0187105  -3.47383 5.1308e-04 NPI.pre <--> NPI.pro    
## gam2  -0.097320 0.0245957  -3.95679 7.5963e-05 AHI.pre <--> AHI.pro    
## gam3   0.016412 0.0220191   0.74533 4.5607e-01 PANAS.pre <--> PANAS.pro
## gam4   0.038816 0.0214969   1.80566 7.0971e-02 CESD.pre <--> CESD.pro  
## b1    -0.042185 0.0401513  -1.05065 2.9342e-01 comp <--- Pro           
## b2    -0.075914 0.0408433  -1.85866 6.3075e-02 comp <--- Pre           
## b3    -0.345418 0.1208306  -2.85869 4.2539e-03 SE <--- Pre             
## b4    -0.050266 0.1174302  -0.42805 6.6861e-01 SE <--- Pro             
## b5     0.245518 0.1349932   1.81874 6.8951e-02 Func <--- Pre           
## b6     0.335489 0.1264449   2.65325 7.9722e-03 Func <--- Pro           
## b7     1.028235 0.1870668   5.49662 3.8714e-08 SE <--- comp            
## b8    -0.120205 0.2092664  -0.57441 5.6569e-01 Func <--- comp          
## b9     0.497185 0.0708686   7.01559 2.2899e-12 Func <--- SE            
## rrCOV -0.789889 0.0419526 -18.82811 4.4433e-79 Pro <--> Pre            
## th3    0.074737 0.0068617  10.89187 1.2603e-27 comp <--> comp          
## th4    0.603685 0.0568900  10.61144 2.6366e-26 SE <--> SE              
## th5    0.679139 0.0642102  10.57682 3.8169e-26 Func <--> Func          

##  Iterations =  143 

i <- 2 ## test
for (i in 1:5){
  assign(paste("i2.sem",i,sep="."),sem(pp.mod.raw,cov(complete(i2.mice,i)),250))
}

## Figure 1:

## edited to eliminate all the double-headed arrows other than the cor between Pro and Pre
## DO NOT RE-RUN
## FINAL MODEL:
## pathDiagram(sem.r1,file="ProPreSEMraw",edge.labels="values",standardize=T,ignore.double=F,same.rank=c(sem.r1$var.names[1:8],sem.r1$var.names[12:13]),min.rank=sem.r1$var.names[1:8])

pathDiagram(i.sem.1,file="ProPreSEM",edge.labels="values",standardize=T,ignore.double=F)
## edit the dot file and then run these commands
system("dot -Tpdf ProPreSEM.dot > ProPreSEM.pdf")
system("dot -Tpng ProPreSEM.dot > ProPreSEM.png")


## eliminate AHI from the model:

pp.mod2.raw <- matrix(c('Pro -> NPI.pro','lam11',NA,
                       'Pro -> PANAS.pro','lam13',NA,
                       'Pro -> CESD.pro','lam14',NA,
                       'Pre -> NPI.pre','lam21',NA,
                       'Pre -> PANAS.pre','lam23',NA,
                       'Pre -> CESD.pre','lam24',NA,
                       'NPI.pro <-> NPI.pro','th11',NA,
                       'PANAS.pro <-> PANAS.pro','th13',NA,
                       'CESD.pro <-> CESD.pro','th14',NA,
                       'NPI.pre <-> NPI.pre','th21',NA,
                       'PANAS.pre <-> PANAS.pre','th23',NA,
                       'CESD.pre <-> CESD.pre','th24',NA,
                       'Pro -> comp','b1',NA,
                       'Pre -> comp','b2',NA,
                       'Pre -> SE','b3',NA,
                       'Pro -> SE','b4',NA,
                        'Pre -> Func','b5',NA,
                        'Pro -> Func','b6',NA,
                        'comp -> SE','b7',NA,
                        'comp -> Func','b8',NA,
                        'SE -> Func','b9',NA,
                        'Pre <-> Pro','rrCOV',NA,
                        'Pre <-> Pre',NA,1,
                        'Pro <-> Pro',NA,1,
                        'comp <-> comp','th3',NA,
                        'SE <-> SE','th4',NA,
                        'Func <-> Func','th5',NA),ncol=3,byrow=T)

sem.r2 <- sem(pp.mod2.raw,cov(complete(i2.mice)[,-c(2,6)]),250)
summary(sem.r2) # 

pathDiagram(sem.r2,file="ProPreSEMraw2FIN",edge.labels="values",standardize=T,ignore.double=F,same.rank=c(sem.r2$var.names[1:6],sem.r2$var.names[10:11]),min.rank=sem.r2$var.names[1:6])
# pathDiagram(sem.r2,file="ProPreSEMraw2FIN",edge.labels="values",standardize=T,ignore.double=F)
## run this if you want to recreate the figure after editing the dot file:
system("dot -Tpdf ProPreSEMraw2FIN.dot > ProPreSEMraw2FIN.pdf")
system("dot -Tpng ProPreSEMraw2FIN.dot > ProPreSEMraw2FIN.png")
## don't run below again or it will screw up my hand edited changes:
## system("dot2tex -f pst ProPreSEMraw.dot > ProPreSEMraw.tex")
# edited the tex file to put subscripts on the MV's for Pro and Pre & cleaned it up.
system("latex ProPreSEMraw.tex")
system("dvips -o SEMfigFIN.ps ProPreSEMraw.dvi")
system("ps2pdf SEMfigFIN.ps")
system("convert SEMfigFIN.ps SEMfigFIN.png")

## Table 1:

## MANIFEST VARIABLE MODEL TABLE

library(xtable)
library(psych)
## xtable(stdCoef(sem.r1,digits=2))

Tab1 <- summary(i.sem.1)$coeff
row.names(Tab1) <- Tab1[,5]
Tab1 <- Tab1[,-5]
Tab1
Tab1 <- cbind(round(Tab1,2)[1:9,],round(stdCoef(i.sem.1,digits=2)[1:9,2],2))
names(Tab1)[5] <- "$Beta$"

Tab1 <- Tab1[,c(5,1:4)]
names(Tab1)[2:5] <- c("$b$","$SE_b$","z","$p-value$")
Tab1

#stdCoef(sem.r1)
#str(summary(sem.r1)$coeff)

Prep <- round(p.rep.r(abs(stdCoef(i.sem.1,digits=2)[1:9,2]),250)$p.rep,2)
Tab1 <- data.frame(Tab1,Prep)
names(Tab1)[6] <- "$prep$"
print(xtable(Tab1),sanitize.colnames.function=NULL,file="Table1FINAL.tex")
print(xtable(Tab1),sanitize.colnames.function=NULL,type="html",file="Table1FINAL.html")



## RAW TABLE

library(xtable)
library(psych)
## xtable(stdCoef(sem.r1,digits=2))

Tab1 <- summary(sem.r1)$coeff
row.names(Tab1) <- Tab1[,5]
Tab1 <- Tab1[,-5]
Tab1
Tab1 <- cbind(round(Tab1,2),round(stdCoef(sem.r1,digits=2)[-c(31,32),2],2))
names(Tab1)[5] <- "$Beta$"

Tab1 <- Tab1[,c(5,1:4)]
names(Tab1)[2:5] <- c("$b$","$SE_b$","z","$p-value$")
Tab1

#stdCoef(sem.r1)
#str(summary(sem.r1)$coeff)

Prep <- round(p.rep.r(abs(stdCoef(sem.r1,digits=2)[-c(31,32),2]),250)$p.rep,2)

Tab1 <- data.frame(Tab1,Prep)
names(Tab1)[6] <- "$p_{rep}$"
print(xtable(Tab1),sanitize.colnames.function=NULL,file="Table1FINAL.tex")
print(xtable(Tab1),sanitize.colnames.function=NULL,type="html",file="Table1FINAL.html")


### INSERT lm() analyses here that address the interaction between Tx Grp and Pro/Pre variables.
NEW.dat <- PPdatF[,c("Pre","Pro","comp","sepainRes","sefcnRes","seosxRes","sf36totRes","sf36pfRes","sf36bpRes","sf36phRes","GLabel","personid")]
se.zfa <- zfa(NEW.dat[,4:6])
func.zfa <- zfa(NEW.dat[,7:10])
NEW.dat <- cbind(NEW.dat[,c(12,11,1:3)],se.zfa$scores,func.zfa$scores)
names(NEW.dat) <- c("personid","GLabel","Pre","Pro","comp","SE","Func")
NEW.dat <- NEW.dat[!is.na(NEW.dat$GLabel),]
NEW.dat$GLabel <- as.factor(NEW.dat$GLabel)
NEW.dat$Pro.bin <- "Low"
NEW.dat$Pro.bin[NEW.dat$Pro >= median(NEW.dat$Pro)] <- "High"
NEW.dat$Pro.bin <- as.factor(NEW.dat$Pro.bin)
NEW.dat$Pre.bin <- "Low"
NEW.dat$Pre.bin[NEW.dat$Pre >= median(NEW.dat$Pre)] <- "High"
NEW.dat$Pre.bin <- as.factor(NEW.dat$Pre.bin)

#NEW.dat$Pro.bin <- cut(NEW.dat$Pro,2,labels=c("Low","High"))
#NEW.dat$Pre.bin <- cut(NEW.dat$Pre,2,labels=c("Low","High"))
NEW.dat$PPcat <- "BothLow"
NEW.dat$PPcat[NEW.dat$Pro.bin=="High" & NEW.dat$Pre.bin=="Low"] <- "Pro"
NEW.dat$PPcat[NEW.dat$Pro.bin=="Low" & NEW.dat$Pre.bin=="High"] <- "Pre"
NEW.dat$PPcat[NEW.dat$Pro.bin=="High" & NEW.dat$Pre.bin=="High"] <- "BothHigh"

lm.INT <- lm(Func~Pro+Pre+GLabel+Pro:GLabel+Pre:GLabel,data=NEW.dat)
summary(lm.INT)

# Call:
#   lm(formula = Func ~ Pro + Pre + GLabel + Pro:GLabel + Pre:GLabel, 
#      data = NEW.dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4846 -0.3814  0.0751  0.5335  3.4153 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                0.11618    0.10246   1.134  0.25819   
# Pro                        0.56576    0.18904   2.993  0.00311 **
# Pre                        0.21554    0.20947   1.029  0.30470   
# GLabelExercise            -0.37314    0.15320  -2.436  0.01573 * 
# GLabelSelf-Management     -0.07166    0.14856  -0.482  0.63005   
# Pro:GLabelExercise        -0.21350    0.30602  -0.698  0.48619   
# Pro:GLabelSelf-Management -0.38116    0.28423  -1.341  0.18141   
# Pre:GLabelExercise        -0.65731    0.34750  -1.892  0.05998 . 
# Pre:GLabelSelf-Management -0.46489    0.32726  -1.421  0.15698   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8803 on 203 degrees of freedom
# Multiple R-squared:  0.1195,  Adjusted R-squared:  0.08475 
# F-statistic: 3.442 on 8 and 203 DF,  p-value: 0.0009704

table(NEW.dat$GLabel,NEW.dat$PPcat)

##                 BothHigh BothLow Pre Pro
## Combined               8      10  25  32
## Exercise              16      12  13  24
## Self-Management       11      13  33  15

table(NEW.dat$Pre.bin)
hist(NEW.dat$Pre)
hist(NEW.dat$Pro)

#### HMMM.  What do we make of the large differences in frequencies by group?

lm.INT2 <- lm(Func~Pro.bin+Pre.bin+GLabel+Pro.bin:GLabel+Pre.bin:GLabel,data=NEW.dat)
summary(lm.INT2)

## MORE DATA

OB <- read.csv("./KNEEfcnOBmi.csv",T)
str(OB)
table(OB$personid,OB$month)
efa1 <- factanal(OB[,8:12],2)
library(psy)
scree.plot(OB[,8:12])
efa1

summary(princomp(OB[,8:12]))

zfa.ob <- zfa(OB[,8:12])
summary(zfa.ob)
OB$obFcn <- zfa.ob$scores
igc.ob <- igc(OB,"personid","month","obFcn")
plot(igc.ob)

library(nlme)
obGrp <- groupedData(obFcn~month|personid,data=OB)

pdf("GrowthCurvesBySubjectOBFcn.pdf")
plot(obGrp)
dev.off()

ob.parms <- coef(igc.ob)

library(lme4)

lme.0 <- lmer(obFcn~1 + (1 | personid),data=OB) # random intercepts
lme.1 <- lmer(obFcn~month + (1 | personid),data=OB) # fixed slope with random ints
lme.2 <- lmer(obFcn~month + (month | personid),data=OB) # random slopes and ints
anova(lme.0,lme.1,lme.2)
plot(lme.1)

## simple change scores may be our best bet

OB.0 <- OB[OB$month==0,c("personid","obFcn")]
names(OB.0) <- c("personid","obFcn.0")
OB.9 <- OB[OB$month==9,c("personid","obFcn")]
names(OB.9) <- c("personid","obFcn.9")
OB.24 <- OB[OB$month==24,c("personid","obFcn")]
names(OB.24) <- c("personid","obFcn.24")
obFcn.dat <- data.frame(personid=OB.0$personid,obFcnChng1=OB.9$obFcn.9-OB.0$obFcn.0,obFcnChng2=OB.24$obFcn.24-OB.0$obFcn.0,obFcnChng3=OB.24$obFcn.24-OB.9$obFcn.9)

str(obFcn.dat)
hist(obFcn.dat$obFcnChng1)

## merge the obFcn.dat into the dataset we used for our Pro/Pre analysis
## Dan will do this....

NEW.dat <- merge(NEW.dat,obFcn.dat,by="personid")
str(NEW.dat)

### OB change 1 analyses -------------

lm.0 <- lm(obFcnChng1~1,data=NEW.dat)
lm.1 <- lm(obFcnChng1~Pro,data=NEW.dat)
lm.2 <- lm(obFcnChng1~Pro+Pre,data=NEW.dat)
lm.3 <- lm(obFcnChng1~Pro*Pre,data=NEW.dat)
lm.4 <- lm(obFcnChng1~Pro*Pre+SE,data=NEW.dat)
lm.5 <- lm(obFcnChng1~Pro*Pre*SE,data=NEW.dat)
lm.6 <- lm(obFcnChng1~GLabel,data=NEW.dat)
anova(lm.0,lm.1,lm.2,lm.3,lm.4,lm.5,lm.6)
anova(lm.1,lm.2)
lm.2a <- lm(obFcnChng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2b <- lm(obFcnChng1~Func,data=NEW.dat)
anova(lm.2,lm.2a,lm.2b)
summary(lm.2a)
summary(lm.6)

# OB change 2 analyses -----------

lm.0 <- lm(obFcnChng2~1,data=NEW.dat)
lm.1 <- lm(obFcnChng2~Pro,data=NEW.dat)
lm.2 <- lm(obFcnChng2~Pro+Pre,data=NEW.dat)
lm.3 <- lm(obFcnChng2~Pro*Pre,data=NEW.dat)
lm.4 <- lm(obFcnChng2~Pro*Pre+SE,data=NEW.dat)
lm.5 <- lm(obFcnChng2~Pro*Pre*SE,data=NEW.dat)
lm.6 <- lm(obFcnChng2~GLabel,data=NEW.dat)
anova(lm.0,lm.1,lm.2,lm.3,lm.4,lm.5,lm.6)
anova(lm.1,lm.4)
lm.4a <- lm(obFcnChng2~Pro*Pre+SE+Func+comp,data=NEW.dat)
lm.4b <- lm(obFcnChng2~Func,data=NEW.dat)
anova(lm.4,lm.4a,lm.4b)
summary(lm.4a)

# OB change 3 analyses -----------

lm.0 <- lm(obFcnChng3~1,data=NEW.dat)
lm.1 <- lm(obFcnChng3~Pro,data=NEW.dat)
lm.2 <- lm(obFcnChng3~Pro+Pre,data=NEW.dat)
lm.3 <- lm(obFcnChng3~Pro*Pre,data=NEW.dat)
lm.4 <- lm(obFcnChng3~Pro*Pre+SE,data=NEW.dat)
lm.5 <- lm(obFcnChng3~Pro*Pre*SE,data=NEW.dat)
lm.6 <- lm(obFcnChng3~GLabel,data=NEW.dat)
anova(lm.0,lm.1,lm.2,lm.3,lm.4,lm.5,lm.6)
anova(lm.1,lm.4)
lm.4a <- lm(obFcnChng3~Pro*Pre+SE+Func+comp,data=NEW.dat)
lm.4b <- lm(obFcnChng3~Func,data=NEW.dat)
anova(lm.4,lm.4a,lm.4b)
summary(lm.3)

## Summary: Pro/Pre does not predict aggregate functioning change measures across any time-points.

#### Individual Objective Functioning Measures

OB.0.5 <- OB[OB$month==0,c("personid","legpress","rom","ergos","getupgo","step5")]
names(OB.0.5) <- c("personid","legpress.0","rom.0","ergos.0","getupgo.0","step5.0")
names(OB.0.5)
OB.9.5 <- OB[OB$month==9,c("personid","legpress","rom","ergos","getupgo","step5")]
names(OB.9.5) <- c("personid","legpress.9","rom.9","ergos.9","getupgo.9","step5.9")
names(OB.9.5)
OB.24.5 <- OB[OB$month==24,c("personid","legpress","rom","ergos","getupgo","step5")]
names(OB.24.5) <- c("personid","legpress.24","rom.24","ergos.24","getupgo.24","step5.24")
names(OB.24.5)
legpressChng.dat <- data.frame(personid=OB.0.5$personid,legpressChng1=OB.9.5$legpress.9-OB.0.5$legpress.0,legpressChng2=OB.24.5$legpress.24-OB.0.5$legpress.0,legpressChng3=OB.24.5$legpress.24-OB.9.5$legpress.9)
names(legpressChng.dat)
romChng.dat <- data.frame(personid=OB.0.5$personid,romChng1=OB.9.5$rom.9-OB.0.5$rom.0,romChng2=OB.24.5$rom.24-OB.0.5$rom.0,romChng3=OB.24.5$rom.24-OB.9.5$rom.9)
names(romChng.dat)
ergosChng.dat <- data.frame(personid=OB.0.5$personid,ergosChng1=OB.9.5$ergos.9-OB.0.5$ergos.0,ergosChng2=OB.24.5$ergos.24-OB.0.5$ergos.0,ergosChng3=OB.24.5$ergos.24-OB.9.5$ergos.9)
names(ergosChng.dat)
getupgoChng.dat <- data.frame(personid=OB.0.5$personid,getupgoChng1=OB.9.5$getupgo.9-OB.0.5$getupgo.0,getupgoChng2=OB.24.5$getupgo.24-OB.0.5$getupgo.0,getupgoChng3=OB.24.5$getupgo.24-OB.9.5$getupgo.9)
names(getupgoChng.dat)
step5Chng.dat <- data.frame(personid=OB.0.5$personid,step5Chng1=OB.9.5$step5.9-OB.0.5$step5.0,step5Chng2=OB.24.5$step5.24-OB.0.5$step5.0,step5Chng3=OB.24.5$step5.24-OB.9.5$step5.9)
names(step5Chng.dat)
str(obFcn.dat)
hist(obFcn.dat$obFcnChng1)

# Merge Individual functioning measures, Pro/Pre, SE, & Comp

NEW.dat <- merge(NEW.dat,legpressChng.dat,by="personid")
NEW.dat <- merge(NEW.dat,romChng.dat,by="personid")
NEW.dat <- merge(NEW.dat,ergosChng.dat,by="personid")
NEW.dat <- merge(NEW.dat,getupgoChng.dat,by="personid")
NEW.dat <- merge(NEW.dat,step5Chng.dat,by="personid")
str(NEW.dat)

# Pro/Pre Predicting legpress during treatment

lm.0l <- lm(legpressChng1~1,data=NEW.dat)
lm.1l <- lm(legpressChng1~Pro,data=NEW.dat)
lm.2l <- lm(legpressChng1~Pro+Pre,data=NEW.dat)
lm.3l <- lm(legpressChng1~Pro*Pre,data=NEW.dat)
lm.4l <- lm(legpressChng1~Pro*Pre+SE,data=NEW.dat)
lm.5l <- lm(legpressChng1~Pro*Pre*SE,data=NEW.dat)
lm.6l <- lm(legpressChng1~GLabel,data=NEW.dat)
anova(lm.0l,lm.1l,lm.2l,lm.3l,lm.4l,lm.5l,lm.6l)
anova(lm.1l,lm.2l)
lm.2al <- lm(legpressChng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bl <- lm(legpressChng1~Func,data=NEW.dat)
anova(lm.2l,lm.2al,lm.2bl)
summary(lm.2al)
summary(lm.6l)

# Pro/Pre Predicting rom during treatment

lm.0r <- lm(romChng1~1,data=NEW.dat)
lm.1r <- lm(romChng1~Pro,data=NEW.dat)
lm.2r <- lm(romChng1~Pro+Pre,data=NEW.dat)
lm.3r <- lm(romChng1~Pro*Pre,data=NEW.dat)
lm.4r <- lm(romChng1~Pro*Pre+SE,data=NEW.dat)
lm.5r <- lm(romChng1~Pro*Pre*SE,data=NEW.dat)
lm.6r <- lm(romChng1~GLabel,data=NEW.dat)
anova(lm.0r,lm.1r,lm.2r,lm.3r,lm.4r,lm.5r,lm.6r)
anova(lm.1r,lm.2r)
lm.2ar <- lm(romChng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2br <- lm(romChng1~Func,data=NEW.dat)
anova(lm.2r,lm.2ar,lm.2br)
summary(lm.2ar)
summary(lm.6r)

# Pro/Pre Predicting ergos during treatment

lm.0e <- lm(ergosChng1~1,data=NEW.dat)
lm.1e <- lm(ergosChng1~Pro,data=NEW.dat)
lm.2e <- lm(ergosChng1~Pro+Pre,data=NEW.dat)
lm.3e <- lm(ergosChng1~Pro*Pre,data=NEW.dat)
lm.4e <- lm(ergosChng1~Pro*Pre+SE,data=NEW.dat)
lm.5e <- lm(ergosChng1~Pro*Pre*SE,data=NEW.dat)
lm.6e <- lm(ergosChng1~GLabel,data=NEW.dat)
anova(lm.0e,lm.1e,lm.2e,lm.3e,lm.4e,lm.5e,lm.6e)
anova(lm.1e,lm.2e)
lm.2ae <- lm(ergosChng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2be <- lm(ergosChng1~Func,data=NEW.dat)
anova(lm.2e,lm.2ae,lm.2be)
summary(lm.2ae)
summary(lm.6e)

# Pro/Pre Predicting getupgo during treatment

lm.0g <- lm(getupgoChng1~1,data=NEW.dat)
lm.1g <- lm(getupgoChng1~Pro,data=NEW.dat)
lm.2g <- lm(getupgoChng1~Pro+Pre,data=NEW.dat)
lm.3g <- lm(getupgoChng1~Pro*Pre,data=NEW.dat)
lm.4g <- lm(getupgoChng1~Pro*Pre+SE,data=NEW.dat)
lm.5g <- lm(getupgoChng1~Pro*Pre*SE,data=NEW.dat)
lm.6g <- lm(getupgoChng1~GLabel,data=NEW.dat)
anova(lm.0g,lm.1g,lm.2g,lm.3g,lm.4g,lm.5g,lm.6g)
anova(lm.1g,lm.2g)
lm.2ag <- lm(getupgoChng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bg <- lm(getupgoChng1~Func,data=NEW.dat)
anova(lm.2g,lm.2ag,lm.2bg)
summary(lm.2ag)
summary(lm.6g)

# Pro/Pre Predicting step5 during treatment

lm.0s <- lm(step5Chng1~1,data=NEW.dat)
lm.1s <- lm(step5Chng1~Pro,data=NEW.dat)
lm.2s <- lm(step5Chng1~Pro+Pre,data=NEW.dat)
lm.3s <- lm(step5Chng1~Pro*Pre,data=NEW.dat)
lm.4s <- lm(step5Chng1~Pro*Pre+SE,data=NEW.dat)
lm.5s <- lm(step5Chng1~Pro*Pre*SE,data=NEW.dat)
lm.6s <- lm(step5Chng1~GLabel,data=NEW.dat)
anova(lm.0s,lm.1s,lm.2s,lm.3s,lm.4s,lm.5s,lm.6s)
anova(lm.1s,lm.2s)
lm.2as <- lm(step5Chng1~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bs <- lm(step5Chng1~Func,data=NEW.dat)
anova(lm.2s,lm.2as,lm.2bs)
summary(lm.2as)
summary(lm.6s)

## Summary: Pro/Pre does not significantly predict individual objective functioning change during treatment.
## Except, prevention marginally predicts step 5 performance.

# Pro/Pre Predicting legpress 0-24 month change

lm.0l <- lm(legpressChng2~1,data=NEW.dat)
lm.1l <- lm(legpressChng2~Pro,data=NEW.dat)
lm.2l <- lm(legpressChng2~Pro+Pre,data=NEW.dat)
lm.3l <- lm(legpressChng2~Pro*Pre,data=NEW.dat)
lm.4l <- lm(legpressChng2~Pro*Pre+SE,data=NEW.dat)
lm.5l <- lm(legpressChng2~Pro*Pre*SE,data=NEW.dat)
lm.6l <- lm(legpressChng2~GLabel,data=NEW.dat)
anova(lm.0l,lm.1l,lm.2l,lm.3l,lm.4l,lm.5l,lm.6l)
anova(lm.1l,lm.2l)
lm.2al <- lm(legpressChng2~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bl <- lm(legpressChng2~Func,data=NEW.dat)
anova(lm.2l,lm.2al,lm.2bl)
summary(lm.2al)
summary(lm.6l)

# Pro/Pre Predicting rom 0-24 month change

lm.0r <- lm(romChng2~1,data=NEW.dat)
lm.1r <- lm(romChng2~Pro,data=NEW.dat)
lm.2r <- lm(romChng2~Pro+Pre,data=NEW.dat)
lm.3r <- lm(romChng2~Pro*Pre,data=NEW.dat)
lm.4r <- lm(romChng2~Pro*Pre+SE,data=NEW.dat)
lm.5r <- lm(romChng2~Pro*Pre*SE,data=NEW.dat)
lm.6r <- lm(romChng2~GLabel,data=NEW.dat)
anova(lm.0r,lm.1r,lm.2r,lm.3r,lm.4r,lm.5r,lm.6r)
anova(lm.1r,lm.2r)
lm.2ar <- lm(romChng2~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2br <- lm(romChng2~Func,data=NEW.dat)
anova(lm.2r,lm.2ar,lm.2br)
summary(lm.2ar)
summary(lm.6r)

# Pro/Pre Predicting ergos 0-24 month change

lm.0e <- lm(ergosChng2~1,data=NEW.dat)
lm.1e <- lm(ergosChng2~Pro,data=NEW.dat)
lm.2e <- lm(ergosChng2~Pro+Pre,data=NEW.dat)
lm.3e <- lm(ergosChng2~Pro*Pre,data=NEW.dat)
lm.4e <- lm(ergosChng2~Pro*Pre+SE,data=NEW.dat)
lm.5e <- lm(ergosChng2~Pro*Pre*SE,data=NEW.dat)
lm.6e <- lm(ergosChng2~GLabel,data=NEW.dat)
anova(lm.0e,lm.1e,lm.2e,lm.3e,lm.4e,lm.5e,lm.6e)
anova(lm.1e,lm.2e)
lm.2ae <- lm(ergosChng2~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2be <- lm(ergosChng2~Func,data=NEW.dat)
anova(lm.2e,lm.2ae,lm.2be)
summary(lm.2ae)
summary(lm.6e)

# Pro/Pre Predicting getupgo 0-24 month change

lm.0g <- lm(getupgoChng2~1,data=NEW.dat)
lm.1g <- lm(getupgoChng2~Pro,data=NEW.dat)
lm.2g <- lm(getupgoChng2~Pro+Pre,data=NEW.dat)
lm.3g <- lm(getupgoChng2~Pro*Pre,data=NEW.dat)
lm.4g <- lm(getupgoChng2~Pro*Pre+SE,data=NEW.dat)
lm.5g <- lm(getupgoChng2~Pro*Pre*SE,data=NEW.dat)
lm.6g <- lm(getupgoChng2~GLabel,data=NEW.dat)
anova(lm.0g,lm.1g,lm.2g,lm.3g,lm.4g,lm.5g,lm.6g)
anova(lm.1g,lm.2g)
lm.2ag <- lm(getupgoChng2~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bg <- lm(getupgoChng2~Func,data=NEW.dat)
anova(lm.2g,lm.2ag,lm.2bg)
summary(lm.2ag)
summary(lm.6g)

# Pro/Pre Predicting step5 0-24 month change

lm.0s <- lm(step5Chng2~1,data=NEW.dat)
lm.1s <- lm(step5Chng2~Pro,data=NEW.dat)
lm.2s <- lm(step5Chng2~Pro+Pre,data=NEW.dat)
lm.3s <- lm(step5Chng2~Pro*Pre,data=NEW.dat)
lm.4s <- lm(step5Chng2~Pro*Pre+SE,data=NEW.dat)
lm.5s <- lm(step5Chng2~Pro*Pre*SE,data=NEW.dat)
lm.6s <- lm(step5Chng2~GLabel,data=NEW.dat)
anova(lm.0s,lm.1s,lm.2s,lm.3s,lm.4s,lm.5s,lm.6s)
anova(lm.1s,lm.2s)
lm.2as <- lm(step5Chng2~Pro+Pre+SE+Func+comp,data=NEW.dat)
lm.2bs <- lm(step5Chng2~Func,data=NEW.dat)
anova(lm.2s,lm.2as,lm.2bs)
summary(lm.2as)
summary(lm.6s)

## Summary:  Pro/Pre does not predicting 0-24 month change in any individual objective functioning measure.
## Pro almost predicts legpress change over 24 months.



# Does Objective Functioning Baseline differ by Pro/Pre?
OBFuncBaseline <- merge(OB.0, NEW.dat, by="personid")

OBFuncBaseline$PPcat <- as.factor(OBFuncBaseline$PPcat)
contrasts(OBFuncBaseline$PPcat) <- contr.treatment(4,4)
contrasts(OBFuncBaseline$PPcat)

ProPreOB.Baseline <- lm(obFcn.0~PPcat,data=OBFuncBaseline)
summary(ProPreOB.Baseline) # Not even close to sig. differences.  This is good.

# Does Subjective Functioning Baseline differ by Pro/Pre?
SubFuncBaseline <- merge(???, NEW.dat, by="personid")

SubFuncBaseline$PPcat <- as.factor(SubFuncBaseline$PPcat)
contrasts(SubFuncBaseline$PPcat) <- contr.treatment(4,4)
contrasts(SubFuncBaseline$PPcat)

ProPreSub.Baseline <- lm(???~PPcat,data=OBFuncBaseline)
summary(ProPreSub.Baseline)

# Predicting Subjective Functioning from Pro/Pre and Objective Functioning
SubFunc <- lm(Func~Pro*Pre*obFcnChng1.x,data=NEW.dat)
summary(SubFunc)

# Correlations between Subjective Functioning and Objective Functioning at 0,9,& 24 Months
SubOB.Func <- merge(???,NEW.dat, by="personid")
rcorr(SubOB.Func)
pairs.panels(SubOB.Func)


# Raw correlations of functioning measures by time

# New correlations for Final Analysis ----

SubFcn.tmp <- knee.l[,c(2,3,17,18,19,25,26)]
SubFcn.zfa <- zfa(SubFcn.tmp[,3:7])$scores 
SubFcn <- data.frame(personid=knee.l[,2],month=knee.l[,3],subFcn=SubFcn.zfa)

SubFcn.0 <- SubFcn[SubFcn$month==0,c(1,3)]
SubFcn.9 <- SubFcn[SubFcn$month==9,c(1,3)]
SubFcn.24 <- SubFcn[SubFcn$month==24,c(1,3)]
names(SubFcn.0) <- c("personid","subFcn.0")
names(SubFcn.9) <- c("personid","subFcn.9")
names(SubFcn.24) <- c("personid","subFcn.24")

FUNC <- merge(SubFcn.0,SubFcn.9,by="personid")
FUNC <- merge(FUNC,SubFcn.24,by="personid")
FUNC <- merge(FUNC,OB.0,by="personid")
FUNC <- merge(FUNC,OB.9,by="personid")
FUNC <- merge(FUNC,OB.24,by="personid")
str(FUNC)

round(cor(FUNC[,-1]),2)

