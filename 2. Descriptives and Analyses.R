options(scipen=99)    #Scientific Notation to fixed instead of exponential.

################################################################################################################################
############################################## DATA SHAPING & COMPUTATION OF VARIABLES #########################################
################################################################################################################################

#Creating ID column. Our CASE variable (CASE_T2) has multiple NAs.
data1$ID <- seq.int(nrow(data1))

##############################################   JUSTICE SENSITIVITY   ################################################

#Calculating and Standardizing aggregated scores of JS.
library(psych)
data1$VictimJS <- rowMeans(data1[c("JS01_01","JS01_02")],na.rm=TRUE)  #Victim JS.
data1$ObserverJS <- rowMeans(data1[c("JS02_01","JS02_02")],na.rm=TRUE)  #Observer JS.
data1$BeneficiaryJS <- rowMeans(data1[c("JS03_01","JS03_02")],na.rm=TRUE) #Beneficiary JS.
data1$PerpetratorJS <- rowMeans(data1[c("JS04_01","JS04_02")],na.rm=TRUE)  #Perpetrator JS.

OJSm <- mean(data1$ObserverJS,na.rm=TRUE)
OJSsd <- sd(data1$ObserverJS,na.rm=TRUE)
PJSm <- mean(data1$PerpetratorJS,na.rm=TRUE)
PJSsd <- sd(data1$PerpetratorJS,na.rm=TRUE)
BJSm <- mean(data1$BeneficiaryJS,na.rm=TRUE)
BJSsd <- sd(data1$BeneficiaryJS,na.rm=TRUE)

##############################################   3PPG: PUNISHMENT/COMPENSATION   ################################################
#CONTINUOUS.
#Calculate Total PUNISHMENT/COMPENSATION in each Round across decisions.
#For Person A allocating from 0 to 4 coins.
data1$R1<-rowSums(data1[,c('DP02_01','DP03_01','DP04_01','DP05_01','DP06_01')],na.rm = TRUE)
data1$R1.z <- scale(data1$R1)[,]
data1$R2<-rowSums(data1[,c('DP10_01','DP11_01','DP12_01','DP13_01','DP14_01')],na.rm = TRUE)
data1$R2.z <- scale(data1$R2)[,]
data1$R3<-rowSums(data1[,c('DP18_01','DP19_01','DP20_01','DP21_01','DP22_01')],na.rm = TRUE)
data1$R3.z <- scale(data1$R3)[,]
data1$R4<-rowSums(data1[,c('DP26_01','DP27_01','DP28_01','DP29_01','DP30_01')],na.rm = TRUE)
data1$R4.z <- scale(data1$R4)[,]

#Forcing to NA those participants that had more than one missing value per Round.
library(dplyr)
which(data1 %>% select('DP02_01','DP03_01','DP04_01','DP05_01','DP06_01') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 1: Participant 119.
data1[119,c("R1","R1.z")]<-NA
which(data1 %>% select('DP10_01','DP11_01','DP12_01','DP13_01','DP14_01') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 2: Participants 16, 119.
data1[16,c("R2","R2.z")]<-NA
data1[119,c("R2","R2.z")]<-NA
which(data1 %>% select('DP18_01','DP19_01','DP20_01','DP21_01','DP22_01') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 3: Participant 119.
data1[119,c("R3","R3.z")]<-NA
which(data1 %>% select('DP26_01','DP27_01','DP28_01','DP29_01','DP30_01') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 4: Participant 119.
data1[119,c("R4","R4.z")]<-NA

data1$R1.Comp<-rowSums(data1[,c('DP02_02','DP03_02','DP04_02','DP05_02','DP06_02')],na.rm = TRUE)
data1$R1.Comp.z <- scale(data1$R1.Comp)[,]
data1$R2.Comp<-rowSums(data1[,c('DP10_02','DP11_02','DP12_02','DP13_02','DP14_02')],na.rm = TRUE)
data1$R2.Comp.z <- scale(data1$R2.Comp)[,]
data1$R3.Comp<-rowSums(data1[,c('DP18_02','DP19_02','DP20_02','DP21_02','DP22_02')],na.rm = TRUE)
data1$R3.Comp.z <- scale(data1$R3.Comp)[,]
data1$R4.Comp<-rowSums(data1[,c('DP26_02','DP27_02','DP28_02','DP29_02','DP30_02')],na.rm = TRUE)
data1$R4.Comp.z <- scale(data1$R4.Comp)[,]

which(data1 %>% select('DP02_02','DP03_02','DP04_02','DP05_02','DP06_02') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 1: Participant 119.
data1[119,c("R1.Comp","R1.Comp.z")]<-NA
which(data1 %>% select('DP10_02','DP11_02','DP12_02','DP13_02','DP14_02') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 2: Participant 119.
data1[119,c("R2.Comp","R2.Comp.z")]<-NA
which(data1 %>% select('DP18_02','DP19_02','DP20_02','DP21_02','DP22_02') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 3: Participant 119.
data1[119,c("R3.Comp","R3.Comp.z")]<-NA
which(data1 %>% select('DP26_02','DP27_02','DP28_02','DP29_02','DP30_02') %>% apply(MARGIN = 1, function(x) sum(is.na(x)))>1)
#Round 4: Participant 119.
data1[119,c("R4.Comp","R4.Comp.z")]<-NA

#For Person A allocating from 0 to 6 coins.
data1$R1b<-rowSums(data1[,c('DP02_01','DP03_01','DP04_01','DP05_01','DP06_01','DP07_01','DP08_01')],na.rm = TRUE)
data1$R2b<-rowSums(data1[,c('DP10_01','DP11_01','DP12_01','DP13_01','DP14_01','DP15_01','DP16_01')],na.rm = TRUE)
data1$R3b<-rowSums(data1[,c('DP18_01','DP19_01','DP20_01','DP21_01','DP22_01','DP23_01','DP24_01')],na.rm = TRUE)
data1$R4b<-rowSums(data1[,c('DP26_01','DP27_01','DP28_01','DP29_01','DP30_01','DP31_01','DP32_01')],na.rm = TRUE)

data1$R1b.Comp<-rowSums(data1[,c('DP02_02','DP03_02','DP04_02','DP05_02','DP06_02','DP07_02','DP08_02')],na.rm = TRUE)
data1$R2b.Comp<-rowSums(data1[,c('DP10_02','DP11_02','DP12_02','DP13_02','DP14_02','DP15_02','DP16_02')],na.rm = TRUE)
data1$R3b.Comp<-rowSums(data1[,c('DP18_02','DP19_02','DP20_02','DP21_02','DP22_02','DP23_02','DP24_02')],na.rm = TRUE)
data1$R4b.Comp<-rowSums(data1[,c('DP26_02','DP27_02','DP28_02','DP29_02','DP30_02','DP31_02','DP32_02')],na.rm = TRUE)

#PUNISHMENT: DICHOTOMOUS.
data1$Punishment.R1<-ifelse(data1$R1>0,1,ifelse(is.na(data1$R1),NA,0))
data1$Punishment.R2<-ifelse(data1$R2>0,1,ifelse(is.na(data1$R1),NA,0))
data1$Punishment.R3<-ifelse(data1$R3>0,1,ifelse(is.na(data1$R1),NA,0))
data1$Punishment.R4<-ifelse(data1$R4>0,1,ifelse(is.na(data1$R1),NA,0))

table(data1$Punishment.R1)/nrow(data1) #Frequencies.
table(data1$Punishment.R2)/nrow(data1)
table(data1$Punishment.R3)/nrow(data1)
table(data1$Punishment.R4)/nrow(data1)

##############################################  INTERVENTION BEHAVIOR (BEHAVIORAL CODING) ################################################
#CONTINUOUS.
intervCol <- c(grep("ntervention_",colnames(data1),value=FALSE))   #Column numbers of Intervention Ratings for every Embezzlement stage.
intervCol
#Original Inter-rater reliability: Weighted Kappa.
#http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
library(irr)
IRR_4 <- kappa2(data1[,intervCol[1]:intervCol[2]], weight = "squared")
IRR_4 #Interrater Reliability for Phase 4.
IRR_5a <- kappa2(data1[,intervCol[3]:intervCol[4]], weight = "squared")
IRR_5a #Interrater Reliability for Phase 5a.
IRR_5b <- kappa2(data1[,intervCol[5]:intervCol[6]], weight = "squared")
IRR_5b #Interrater Reliability for Phase 5a. No variance on Rater 1 (D).
IRR_6a <- kappa2(data1[,intervCol[7]:intervCol[8]], weight = "squared")
IRR_6a #Interrater Reliability for Phase 6a.
IRR_6b <- kappa2(data1[,intervCol[9]:intervCol[10]], weight = "squared")
IRR_6b #Interrater Reliability for Phase 6b.
IRR_6c <- kappa2(data1[,intervCol[11]:intervCol[12]], weight = "squared")
IRR_6c #Interrater Reliability for Phase 6c. No variance on Rater 1 (D) and Rater 2 (IG).
IRR_6d <- kappa2(data1[,intervCol[13]:intervCol[14]], weight = "squared")
IRR_6d #Interrater Reliability for Phase 6c.
IRR_8 <- kappa2(data1[,intervCol[15]:intervCol[16]], weight = "squared")
IRR_8 #Interrater Reliability for Phase 6c.


#Original Inter-rater reliability: ICC (2,1) Absolute Agreement.
IRR_4 <- icc(data1[,intervCol[1]:intervCol[2]],model="twoway",type="agreement",unit="single")
IRR_4 #Interrater Reliability for Phase 4.
IRR_5a <- icc(data1[,intervCol[3]:intervCol[4]],model="twoway",type="agreement",unit="single")
IRR_5a #Interrater Reliability for Phase 5a.
IRR_5b <- icc(data1[,intervCol[5]:intervCol[6]],model="twoway",type="agreement",unit="single")
data1[,intervCol[5]] #No variance on Rater 1 (D).
IRR_5b #Interrater Reliability for Phase 5b.
IRR_6a <- icc(data1[,intervCol[7]:intervCol[8]],model="twoway",type="agreement",unit="single")
IRR_6a #Interrater Reliability for Phase 6a.
IRR_6b <- icc(data1[,intervCol[9]:intervCol[10]],model="twoway",type="agreement",unit="single")
IRR_6b #Interrater Reliability for Phase 6b.
IRR_6c <- icc(data1[,intervCol[11]:intervCol[12]],model="twoway",type="agreement",unit="single")
data1[,intervCol[11]] #No variance on Rater 1 (D).
data1[,intervCol[12]] #No variance on Rater 2 (IG).
IRR_6c #Interrater Reliability for Phase 6c.
IRR_6d <- icc(data1[,intervCol[13]:intervCol[14]],model="twoway",type="agreement",unit="single")
IRR_6d #Interrater Reliability for Phase 6d.
IRR_8 <- icc(data1[,intervCol[15]:intervCol[16]],model="twoway",type="agreement",unit="single")
IRR_8 #Interrater Reliability for Phase 8.

#Add InterventionRecode by Amelie and Julia of cases of interrater disagreement. 
#See SPSS Syntax "Intervention (Recode Syntax - Julia).sps".
interv.recode <- read.csv("Intervention (Recoded).csv",header=TRUE,sep=";",dec=",",na.strings=c("NA","","-9"),row.names=NULL)
interv.recode[,2:10]<-NULL
colnames(interv.recode)[colnames(interv.recode)=="Intervention_vid"] <- "Interv_recoded"
colnames(interv.recode)[colnames(interv.recode)=="ï..TN"] <- "TN"
data1<-merge(interv.recode,data1,by="TN",all.y=T)

#Values 99 in Intervention_vid belong to disagreements between raters and were recoded by Amelie and Julia. 
#The remaining cases were not recoded because they were excluded cases based on Exclusion_doubts.

#Create Phase 7 - Project Leader's rating when there was no videorecording.
data1$InterStrength_7_1 <- ifelse(data1$RF04_A_06==1,1,0)
data1$InterStrength_7_2 <- ifelse(data1$RF04_A_07==1,2,0)
data1$InterStrength_7_3 <- ifelse(data1$RF04_A_08==1,3,0)

data1$InterStrength_7 <- rowSums(data1[,grep("InterStrength_7_",colnames(data1),value=FALSE)],na.rm = FALSE)

data1[,grep("InterStrength_7_",colnames(data1),value=FALSE)]<-NULL #Delete intermediate variables.

#Maximum Intervention Strength across Phases.
data1$MAXInterStrength <- apply(data1[,c("Interv_recoded","InterStrength_7")],1,max,na.rm=TRUE)
data1$MAXInterStrength <- replace(data1$MAXInterStrength,data1$MAXInterStrength=='-Inf', NA)

table(data1$MAXInterStrength) #NOTE: Frequencies without exclusion based on Doubts about the staged Embezzlement.

#DICHOTOMOUS.
data1$InterDich <- ifelse(is.na(data1$MAXInterStrength),NA, ifelse(data1$MAXInterStrength==0,0,1))
table(data1$InterDich) #NOTE: Frequencies without exclusion based on Doubts about the staged Embezzlement.

##############################################  INTERVENTION BEHAVIOR (RETROSPECTIVE EXTERNAL EVALUATION)  ################################################
#DICHOTOMOUS: Retrospective evaluation from EX1, EX2 (Experimenter 1 and 2),
#CON (Confederate), and PL (Project leader).
# ifelse from 1 = Intervention, 2 = No intervention, to: 1 = Intervention, 0 = No intervention.
data1$EX1_A_06 <- ifelse(data1$RF01_A_06==1,1,0)
data1$EX1_A_07 <- ifelse(data1$RF01_A_07==1,1,0)
data1$EX1_A_08 <- ifelse(data1$RF01_A_08==1,1,0)
data1$EX1_B_06 <- ifelse(data1$RF01_B_06==1,1,0)
data1$EX1_B_07 <- ifelse(data1$RF01_B_07==1,1,0)
data1$EX1_B_08 <- ifelse(data1$RF01_B_08==1,1,0)
data1$EX1_C_06 <- ifelse(data1$RF01_C_06==1,1,0)
data1$EX1_C_07 <- ifelse(data1$RF01_C_07==1,1,0)
data1$EX1_C_08 <- ifelse(data1$RF01_C_08==1,1,0)
data1$EX1_D_06 <- ifelse(data1$RF01_D_06==1,1,0)
data1$EX1_D_07 <- ifelse(data1$RF01_D_07==1,1,0)
data1$EX1_D_08 <- ifelse(data1$RF01_D_08==1,1,0)
data1$EX2_A_06 <- ifelse(data1$RF02_A_06==1,1,0)
data1$EX2_A_07 <- ifelse(data1$RF02_A_07==1,1,0)
data1$EX2_A_08 <- ifelse(data1$RF02_A_08==1,1,0)
data1$CON_A_06 <- ifelse(data1$RF03_A_06==1,1,0)
data1$CON_A_07 <- ifelse(data1$RF03_A_07==1,1,0)
data1$CON_A_08 <- ifelse(data1$RF03_A_08==1,1,0)
data1$CON_B_06 <- ifelse(data1$RF03_B_06==1,1,0)
data1$CON_B_07 <- ifelse(data1$RF03_B_07==1,1,0)
data1$CON_B_08 <- ifelse(data1$RF03_B_08==1,1,0)
data1$CON_C_06 <- ifelse(data1$RF03_C_06==1,1,0) #Typo in one participant, 22 = 2.
data1$CON_C_07 <- ifelse(data1$RF03_C_07==1,1,0)
data1$CON_C_08 <- ifelse(data1$RF03_C_08==1,1,0)
data1$CON_D_06 <- ifelse(data1$RF03_D_06==1,1,0)
data1$CON_D_07 <- ifelse(data1$RF03_D_07==1,1,0)
data1$CON_D_08 <- ifelse(data1$RF03_D_08==1,1,0)
data1$CON_E_06 <- ifelse(data1$RF03_E_06==1,1,0)
data1$CON_E_07 <- ifelse(data1$RF03_E_07==1,1,0)
data1$CON_E_08 <- ifelse(data1$RF03_E_08==1,1,0)
data1$PL_A_06 <- ifelse(data1$RF04_A_06==1,1,0)
data1$PL_A_07 <- ifelse(data1$RF04_A_07==1,1,0)
data1$PL_A_08 <- ifelse(data1$RF04_A_08==1,1,0)

data1$Inter_Retro.Sum <- rowSums(data1[c("EX1_A_06", "EX1_A_07", "EX1_A_08",
                                                 "EX1_B_06", "EX1_B_07", "EX1_B_08",
                                                 "EX1_C_06", "EX1_C_07", "EX1_C_08",
                                                 "EX1_D_06", "EX1_D_07", "EX1_D_08",
                                                 "EX2_A_06", "EX2_A_07", "EX2_A_08",
                                                 "CON_A_06", "CON_A_07", "CON_A_08",
                                                 "CON_B_06", "CON_B_07", "CON_B_08",
                                                 "CON_C_06", "CON_C_07", "CON_C_08",
                                                 "CON_D_06", "CON_D_07", "CON_D_08",
                                                 "CON_E_06", "CON_E_07", "CON_E_08",
                                                 "PL_A_06", "PL_A_07", "PL_A_08")],
                                     na.rm=TRUE)

data1$InterDich_Retro <- ifelse(data1$Inter_Retro.Sum==0,0,1)
table(data1$InterDich_Retro)

#CONTINUOUS.
#From dichotomous YES/NO, create continuous variable from 1 to 3, depending on whether people
#1 = make a comment without labeling the situation as wrong (_06), 
#2 = label the situation as wrong, immoral or fraud (_07),
#3 = Stop the fraud, threat to report it (_08).

data1$EX1_A_06c <- recode(data1$RF01_A_06, '1'=1, '2'=0,.missing=0)
data1$EX1_A_07c <- recode(data1$RF01_A_07, '1'=2, '2'=0,.missing=0)
data1$EX1_A_08c <- recode(data1$RF01_A_08, '1'=3, '2'=0,.missing=0)
data1$EX1_B_06c <- recode(data1$RF01_B_06, '1'=1, '2'=0,.missing=0)
data1$EX1_B_07c <- recode(data1$RF01_B_07, '1'=2, '2'=0,.missing=0)
data1$EX1_B_08c <- recode(data1$RF01_B_08, '1'=3, '2'=0,.missing=0)
data1$EX1_C_06c <- recode(data1$RF01_C_06, '1'=1, '2'=0,.missing=0)
data1$EX1_C_07c <- recode(data1$RF01_C_07, '1'=2, '2'=0,.missing=0)
data1$EX1_C_08c <- recode(data1$RF01_C_08, '1'=3, '2'=0,.missing=0)
data1$EX1_D_06c <- recode(data1$RF01_D_06, '1'=1, '2'=0,.missing=0)
data1$EX1_D_07c <- recode(data1$RF01_D_07, '1'=2, '2'=0,.missing=0)
data1$EX1_D_08c <- recode(data1$RF01_D_08, '1'=3, '2'=0,.missing=0)
data1$EX2_A_06c <- recode(data1$RF02_A_06, '1'=1, '2'=0,.missing=0)
data1$EX2_A_07c <- recode(data1$RF02_A_07, '1'=2, '2'=0,.missing=0)
data1$EX2_A_08c <- recode(data1$RF02_A_08, '1'=3, '2'=0,.missing=0)
data1$CON_A_06c <- recode(data1$RF03_A_06, '1'=1, '2'=0,.missing=0)
data1$CON_A_07c <- recode(data1$RF03_A_07, '1'=2, '2'=0,.missing=0)
data1$CON_A_08c <- recode(data1$RF03_A_08, '1'=3, '2'=0,.missing=0)
data1$CON_B_06c <- recode(data1$RF03_B_06, '1'=1, '2'=0,.missing=0)
data1$CON_B_07c <- recode(data1$RF03_B_07, '1'=2, '2'=0,.missing=0)
data1$CON_B_08c <- recode(data1$RF03_B_08, '1'=3, '2'=0,.missing=0)
data1$CON_C_06c <- recode(data1$RF03_C_06, '1'=1, '2'=0, '22'=0,.missing=0) #One Typo, 22 = 2, i.e., No Intervention.
data1$CON_C_07c <- recode(data1$RF03_C_07, '1'=2, '2'=0,.missing=0)
data1$CON_C_08c <- recode(data1$RF03_C_08, '1'=3, '2'=0,.missing=0)
data1$CON_D_06c <- recode(data1$RF03_D_06, '1'=1, '2'=0,.missing=0)
data1$CON_D_07c <- recode(data1$RF03_D_07, '1'=2, '2'=0,.missing=0)
data1$CON_D_08c <- recode(data1$RF03_D_08, '1'=3, '2'=0,.missing=0)
data1$CON_E_06c <- recode(data1$RF03_E_06, '1'=1, '2'=0,.missing=0)
data1$CON_E_07c <- recode(data1$RF03_E_07, '1'=2, '2'=0,.missing=0)
data1$CON_E_08c <- recode(data1$RF03_E_08, '1'=3, '2'=0,.missing=0)
data1$PL_A_06c <- recode(data1$RF04_A_06, '1'=1, '2'=0,.missing=0)
data1$PL_A_07c <- recode(data1$RF04_A_07, '1'=2, '2'=0,.missing=0)
data1$PL_A_08c <- recode(data1$RF04_A_08, '1'=3, '2'=0,.missing=0)

data1$MAXInterStrength_Retro <-  apply(data1[c("EX1_A_06c", "EX1_A_07c", "EX1_A_08c",
                                                       "EX1_B_06c", "EX1_B_07c", "EX1_B_08c",
                                                       "EX1_C_06c", "EX1_C_07c", "EX1_C_08c",
                                                       "EX1_D_06c", "EX1_D_07c", "EX1_D_08c",
                                                       "EX2_A_06c", "EX2_A_07c", "EX2_A_08c",
                                                       "CON_A_06c", "CON_A_07c", "CON_A_08c",
                                                       "CON_B_06c", "CON_B_07c", "CON_B_08c",
                                                       "CON_C_06c", "CON_C_07c", "CON_C_08c",
                                                       "CON_D_06c", "CON_D_07c", "CON_D_08c",
                                                       "CON_E_06c", "CON_E_07c", "CON_E_08c",
                                                       "PL_A_06c", "PL_A_07c", "PL_A_08c")],1,max)

table(data1$MAXInterStrength_Retro)


##############################################   FEAR OF INVALIDITY  ################################################
data1$PF01_12 <- recode(data1$PF01_12, '1'=6, '2'=5,'3'=4,'4'=3,'5'=2,'6'=1,.missing=0) #In the logfile PF01_12 does not appear as reversed, but it should be reversed-coded.
library(psych)
data1$Fear_of_Invalidity <- rowMeans(data1[c("PF01_12","PF01_13","PF01_14","PF01_15")],na.rm=TRUE)
data1$Fear_of_Invalidity.z <- scale(data1$Fear_of_Invalidity)[,]  #Standardized Fear_of_Invalidity.



##############################################  DATA RESHAPE: WIDE TO LONG FORMAT  ################################################
#Reshape from wide to long format. Individually for each Categorical Variable.
############PUNISHMENT DICHOTOMOUS##########
library(reshape2)
dich1 <- melt(data1,
              id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                        "ObserverJS","BeneficiaryJS","PerpetratorJS"),
                        #"Fear_of_Invalidity.z"),
                        #"MAXInterStrength","InterDich","InterDich_Retro","MAXInterStrength_Retro","Exclusion_doubts"), #ID variables - variables to keep but not split apart on.
              measure.vars=c('Punishment.R1','Punishment.R2','Punishment.R3','Punishment.R4'), #Categories.
              variable.name= "Ambiguity", #Name of categorical variable that defines each within-subject condition.
              value.name="Punishment" #Name of DV.
)
dich2 <- melt(data1,
              id.vars=c("ID"),
              measure.vars=c('Punishment.R1','Punishment.R2','Punishment.R3','Punishment.R4'), 
              variable.name= "Uncertainty", 
              value.name="Punishment"
)

#Specifying Categorical Factors 'AMBIGUITY' and 'UNCERTAINTY' and its Levels.
dich1$Ambiguity <- as.factor(dich1$Ambiguity)
dich2$Uncertainty <- as.factor(dich2$Uncertainty)
library(plyr)
dich1$Ambiguity<-revalue(dich1$Ambiguity, c("Punishment.R1"=0, "Punishment.R2"=0,"Punishment.R3"=1,"Punishment.R4"=1))
dich2$Uncertainty<-revalue(dich2$Uncertainty, c("Punishment.R1"=0, "Punishment.R2"=1,"Punishment.R3"=0,"Punishment.R4"=1))

#Merging both categorical variables in the same dataframe.
dichm <- merge(dich1, dich2,by="row.names")

#Creating Standardized Variables.
dichm$ObserverJS.z <- scale(dichm$ObserverJS)[,] #Standardized Observer JS.
dichm$BeneficiaryJS.z <- scale(dichm$BeneficiaryJS)[,] #Standardized Beneficiary JS.
dichm$PerpetratorJS.z <- scale(dichm$PerpetratorJS)[,]  #Perpetrator JS.

#Creating Subset for H3 - Embezzlement Analyses.
table(is.na(data1$Exclusion_doubts)) #From 164, 28 NAs in Doubts about the Staged Embezzlement.
table(data1$Exclusion_doubts) #From the remaning 136, 32 Participants detected or expressed doubts about the Embezzlement.
dichm_Intervention <- filter(dichm, Exclusion_doubts==0) #N = 104 * 4 = 416.

############PUNISHMENT CONTINUOUS###########
library(reshape2)
df1 <- melt(data1,
            id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                      "ObserverJS","BeneficiaryJS","PerpetratorJS"),
                      #"Fear_of_Invalidity.z"),
                      #"MAXInterStrength","InterDich","InterDich_Retro","MAXInterStrength_Retro","Exclusion_doubts"), #ID variables - variables to keep but not split apart on.
            measure.vars=c('R1','R2','R3','R4'), #Categories.
            variable.name= "Ambiguity", #Name of categorical variable that defines each within-subject condition.
            value.name="Punishment" #Name of DV.
)
df2 <- melt(data1,
            id.vars=c("ID"),
            measure.vars=c('R1','R2','R3','R4'), 
            variable.name= "Uncertainty", 
            value.name="Punishment"
)

#Specifying Categorical Factors 'AMBIGUITY' and 'UNCERTAINTY' and its Levels.
df1$Ambiguity <- as.factor(df1$Ambiguity)
df2$Uncertainty <- as.factor(df2$Uncertainty)
library(plyr)
df1$Ambiguity<-revalue(df1$Ambiguity, c("R1"=0, "R2"=0,"R3"=1,"R4"=1))
df2$Uncertainty<-revalue(df2$Uncertainty, c("R1"=0, "R2"=1,"R3"=0,"R4"=1))

#Merging both categorical variables in the same dataframe.
dm <- merge(df1, df2,by="row.names")
dm$Punishment.y <- NULL
colnames(dm)[colnames(dm)=="Punishment.x"] <- "Punishment"

#Creating Standardized Variables.
dm$ObserverJS.z <- scale(dm$ObserverJS)[,] #Standardized Observer JS.
dm$BeneficiaryJS.z <- scale(dm$BeneficiaryJS)[,] #Standardized Beneficiary JS.
dm$PerpetratorJS.z <- scale(dm$PerpetratorJS)[,]  #Perpetrator JS.
dm$Punishment.z <- scale(dm$Punishment)[,] #Punishment.


#Creating Subset for H3 - Embezzlement Analyses.
table(is.na(data1$Exclusion_doubts)) #From 164, 28 NAs in Doubts about the Staged Embezzlement.
table(data1$Exclusion_doubts) #From the remaning 136, 32 Participants detected or expressed doubts about the Embezzlement.
dm_Intervention <- filter(dm, Exclusion_doubts==0) #N = 104 * 4 = 416.

############COMPENSATION CONTINUOUS###########
library(reshape2)
df3 <- melt(data1,
            id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                      "ObserverJS","BeneficiaryJS","PerpetratorJS"),
                      #"Fear_of_Invalidity.z",
                      #"MAXInterStrength","InterDich","InterDich_Retro","MAXInterStrength_Retro","Exclusion_doubts"), #ID variables - variables to keep but not split apart on.
            measure.vars=c('R1.Comp','R2.Comp','R3.Comp','R4.Comp'), #Categories.
            variable.name= "Ambiguity", #Name of categorical variable that defines each within-subject condition.
            value.name="Compensation" #Name of DV.
)
df4 <- melt(data1,
            id.vars=c("ID"),
            measure.vars=c('R1.Comp','R2.Comp','R3.Comp','R4.Comp'), 
            variable.name= "Uncertainty", 
            value.name="Compensation"
)

#Specifying Categorical Factors 'AMBIGUITY' and 'UNCERTAINTY' and its Levels.
df3$Ambiguity <- as.factor(df3$Ambiguity)
df4$Uncertainty <- as.factor(df4$Uncertainty)
library(plyr)
df3$Ambiguity<-revalue(df3$Ambiguity, c("R1.Comp"=0, "R2.Comp"=0,"R3.Comp"=1,"R4.Comp"=1))
df4$Uncertainty<-revalue(df4$Uncertainty, c("R1.Comp"=0, "R2.Comp"=1,"R3.Comp"=0,"R4.Comp"=1))

#Merging both categorical variables in the same dataframe.
dm2 <- merge(df3, df4,by="row.names")
dm2$Compensation.y <- NULL
colnames(dm2)[colnames(dm2)=="Compensation.x"] <- "Compensation"

#Creating Standardized Variables.
dm2$ObserverJS.z <- scale(dm2$ObserverJS)[,] #Standardized Observer JS.
dm2$BeneficiaryJS.z <- scale(dm2$BeneficiaryJS)[,] #Standardized Beneficiary JS.
dm2$PerpetratorJS.z <- scale(dm2$PerpetratorJS)[,]  #Perpetrator JS.
dm2$Compensation.z <- scale(dm2$Compensation)[,] #Compensation.

#Creating Subset for H3 - Embezzlement Analyses.
table(is.na(data1$Exclusion_doubts)) #From 164, 28 NAs in Doubts about the Staged Embezzlement.
table(data1$Exclusion_doubts) #From the remaning 136, 32 Participants detected or expressed doubts about the Embezzlement.
dm2_Intervention <- filter(dm2, Exclusion_doubts==0) #N = 104 * 4 = 416.

################################################################################################################################
##############################################    DESCRIPTIVES STATISTICS    ################################################
################################################################################################################################

##SOCIODEMOGRAPHICS.
#GENDER.
data1$SD01_02 <- factor(data1$SD01_02,levels = c(1,2), labels = c("Male","Female"))
table(data1$SD01_02)/nrow(data1)
#AGE.
summary(data1$SD01_01)
sd(data1$SD01_01,na.rm = TRUE)
#STUDIES.
table(data1$SD01_03)

#Plot for mapping missing values.
install.packages("Amelia")
library(Amelia)
missmap(data1, main = "Missing values vs observed") 

################ DEPENDENT MEASURES ################
#Dichotomous Punishment, probabilities per treatment.
library(plyr)
#Probabilities for Ambiguity Manipulation.
freq1<-count(dichm,c("Ambiguity","Punishment.x"))
freq1$Prob.<-freq1$freq/sum(dich1$Ambiguity==0)
freq1
#Probabilities for Uncertainty Manipulation.
freq2<-count(dichm,c("Uncertainty","Punishment.x"))
freq2$Prob.<-freq2$freq/sum(dich2$Uncertainty==0,na.rm = TRUE)
freq2

#Punishment and Compensation means in each Round.
mean(data1$R1,na.rm=TRUE)
mean(data1$R2,na.rm=TRUE)
mean(data1$R3,na.rm=TRUE)
mean(data1$R4,na.rm=TRUE)

mean(data1$R1.Comp,na.rm=TRUE)
mean(data1$R2.Comp,na.rm=TRUE)
mean(data1$R3.Comp,na.rm=TRUE)
mean(data1$R4.Comp,na.rm=TRUE)


################# JUSTICE SENSITIVITY ################
#Bivariate correlations of different JS.
library(Hmisc)
JScorr <- rcorr(as.matrix(data1[,c("VictimJS","ObserverJS","BeneficiaryJS","PerpetratorJS")]))
JScorr

#Bivariate correlations with DVs.
# 3PPG: R1, R2, R3, R4, R1.Comp, R2.Comp, R3.Comp, R4.Comp.
rcorr(as.matrix(data1[,c("R1.z","R2.z","R3.z","R4.z","ObserverJS","BeneficiaryJS","PerpetratorJS")]))
rcorr(as.matrix(data1[,c("R1.Comp.z","R2.Comp.z","R3.Comp.z","R4.Comp.z","ObserverJS","BeneficiaryJS","PerpetratorJS")]))

# INTERVENTION: "MAXInterStrength","InterDich","InterDich_Retro","MAXInterStrength_Retro"
rcorr(as.matrix(data1[,c("MAXInterStrength","MAXInterStrength_Retro","ObserverJS","BeneficiaryJS","PerpetratorJS")]))


#Chronbachs alpha.
library(psych)
JSItems <- grep("JS0",colnames(data1),value=FALSE) #Number of Columns of JS Items, including VictimJS.
alpha(as.matrix(data1[,JSItems[7]:JSItems[8]]))  #Reliability for JS scale, excluding VictimJS (i.e., 6 items).
#Alpha = 0.80.

#Simulation for plotting Normality of JS.
plotnormality <- function(var,n=10000){
  m <- mean(var,na.rm = TRUE)
  sd <- sd(var,na.rm = TRUE)
  nsample <- rnorm(n,m,sd)
  library(ggplot2)
  datasim <- data.frame(nsample)
  hist <- ggplot(datasim, aes(x = nsample), binwidth = 2) + 
    geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
    geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + 
    ylab(expression(bold('Density')))
  print(hist)
}

OJSnorm <- plotnormality(data1$ObserverJS.c)
BJSnorm <- plotnormality(data1$BeneficiaryJS.c)
PJSnorm <- plotnormality(data1$PerpetratorJS.c)

#Skewness and Kurtosis.
library(moments)
skewness(OJS.sim)
kurtosis(OJS.sim)
hist(OJS.sim)

#################FEAR OF INVALIDITY###################
#Bivariate correlations of different JS.
library(Hmisc)
rcorr(as.matrix(data1[,c("Fear_of_Invalidity","VictimJS","ObserverJS","BeneficiaryJS","PerpetratorJS")]))


#Bivariate correlations with DVs.
# 3PPG: R1, R2, R3, R4, R1.Comp, R2.Comp, R3.Comp, R4.Comp.
rcorr(as.matrix(data1[,c("Fear_of_Invalidity","R1.z","R2.z","R3.z","R4.z")]))
rcorr(as.matrix(data1[,c("Fear_of_Invalidity","R1.Comp.z","R2.Comp.z","R3.Comp.z","R4.Comp.z")]))

# INTERVENTION: "MAXInterStrength","InterDich","InterDich_Retro","MAXInterStrength_Retro"
rcorr(as.matrix(data1[,c("Fear_of_Invalidity","MAXInterStrength","MAXInterStrength_Retro")]))


#Chronbachs alpha.
library(psych)
FoIitems <- grep("PF01_",colnames(data1),value=FALSE) #Number of Columns of FoI items.
alpha(as.matrix(data1[,FoIitems[1]:FoIitems[4]]))  #Reliability for FoI scale.
#Alpha = .

################################################################################################################################
##############################################           MAIN ANALYSIS          ################################################
################################################################################################################################

library(effects)
library(lme4)
library(lmerTest)
library(lattice)
library(sjPlot)
library(sjstats)



## H1 ## Main effects of Ambiguity and Uncertainty.

H1.RI <- glmer(Punishment.x~Ambiguity + Uncertainty + (1|ID), data=dichm,family = binomial(link = logit))
outcome.H1.RI<-summary(H1.RI)
summary(H1.RI)

#Transformation of logits to ORs.
H1CIs <- confint(H1.RI,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H1ORs <- cbind(Coeff.=fixef(H1.RI),pvalue=outcome.H1.RI$coefficients[,4],OR=exp(fixef(H1.RI)),exp(H1CIs)) #Transform into ORs.
H1ORs

#Transformation of logits to Probabilities.
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob((summary(H1.RI))$coefficients[,1])

library(sjPlot)
sjt.glmer(H1.RI)  #HTML Table of main results.


#Random slopes.
H1.RS1 <- glmer(Punishment.x~Ambiguity + Uncertainty + (1+Ambiguity|ID), data=dichm,family = binomial(link = logit))
summary(H1.RS1)

H1.RS2 <- glmer(Punishment.x~Ambiguity + Uncertainty + (1+Uncertainty|ID), data=dichm,family = binomial(link = logit))
summary(H1.RS2)

H1.RS3 <- glmer(Punishment.x~Ambiguity + Uncertainty + (1+Uncertainty+Ambiguity|ID), data=dichm,family = binomial(link = logit))
summary(H1.RS3)

anova(H1.RS1,H1.RS2) ##MODEL COMPARISON.



## H2a ## Moderation of JSs.
H2a<- glmer(Punishment.x~Ambiguity*PerpetratorJS.z+Ambiguity*ObserverJS.z+ (1|ID), data=dichm,family = binomial(link = logit)) 
summary(H2a)
plot(allEffects(H2a))

H2aCIs <- confint(H2a,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aORs <- cbind(Coeff.=fixef(H2a),OR=exp(fixef(H2a)),exp(H2aCIs))
H2aORs

##H2a ## Simple Slope Analysis.
library(plyr)
dichm$HighOJS.z <- dichm$ObserverJS.z-1
dichm$LowOJS.z <- dichm$ObserverJS.z+1
dichm$Ambiguity.c<-revalue(dichm$Ambiguity, c('0'='-0.5','1'='0.5'))
dichm$Uncertainty.c<-revalue(dichm$Uncertainty, c('0'='-0.5','1'='0.5'))

H2a.high<- glmer(Punishment.x~Ambiguity.c*HighOJS.z+ (1|ID), data=dichm,family = binomial(link = logit))
summary(H2a.high)
H2aHCIs <- confint(H2a.high,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aHORs <- cbind(Coeff.=fixef(H2a.high),OR=exp(fixef(H2a.high)),exp(H2aHCIs))
H2aHORs

H2a.low<- glmer(Punishment.x~Ambiguity.c*LowOJS.z+ (1|ID), data=dichm,family = binomial(link = logit))
summary(H2a.low)
H2aLCIs <- confint(H2a.low,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aLORs <- cbind(Coeff.=fixef(H2a.low),OR=exp(fixef(H2a.low)),exp(H2aLCIs))
H2aLORs


##H2b##

H2b<- glmer(Punishment.x~Uncertainty*BeneficiaryJS.z+ (1|ID), data=dichm,family = binomial(link = logit)) 
summary(H2b)
plot(allEffects(mod2b))

H2bCIs <- confint(H2b,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2bORs <- cbind(Coeff.=fixef(H2b),OR=exp(fixef(H2b)),exp(H2bCIs))
H2bORs

##H3##
#Exclude cases based on Exclusion_Doubts.


#DV = Behavioral Coding.
H3_BC <- glmer(InterDich~Punishment.x*Ambiguity+Punishment.x*Uncertainty + (1|ID), data=dichm_Intervention,family = binomial(link = logit))
summary(H3_BC)

#DV = Retrospective External Evaluation.
H3_Retro <- glmer(InterDich_Retro~Punishment.x*Ambiguity+Punishment.x*Uncertainty + (1|ID), data=dichm_Intervention,family = binomial(link = logit))
summary(H3_Retro)


##Plotting Effects from Logistic Regression.
http://data.library.virginia.edu/visualizing-the-effects-of-logistic-regression/
  
  

################################################################################################################################
##############################################           EXPLORATORY ANALYSIS          #########################################
################################################################################################################################

##H1##
EP1 <- lmer(Punishment.z~Ambiguity+Uncertainty + (1|ID), data=dm) 
summary(EP1)

TEST <- lmer(Punishment.z~Ambiguity+Uncertainty + (1+Ambiguity|ID), data=dm)
summary(TEST)

EC1 <- lmer(Compensation.z~Ambiguity+Uncertainty + (1|ID), data=dm2) 
summary(EC1)


##H2a##
EP2a <- lmer(Punishment.z~Ambiguity*PerpetratorJS.z+Ambiguity*ObserverJS.z + (1|ID), data=dm) 
summary(EP2a)
plot(allEffects(EP2a))

EC2a <- lmer(Compensation.z~Ambiguity*PerpetratorJS.z+Ambiguity*ObserverJS.z + (1|ID), data=dm2) 
summary(EC2a)
plot(allEffects(EC2a))


#Test with Fear of Invalidity.
FearInvalidity <- lmer(Punishment.z.x~Ambiguity*PerpetratorJS.z+Ambiguity*ObserverJS.z + Ambiguity*Fear_of_Invalidity.z+(1|ID), data=dm) 
summary(FearInvalidity)
plot(allEffects(EP2a))

#Model por Power Simulation:
EP2POWER <- lmer(Punishment.z~Ambiguity*ObserverJS.z + (1|ID), data=dm) 
summary(EP2POWER)

EP2POWER_threeway <- lmer(Punishment.x~Ambiguity*Uncertainty*ObserverJS.z + (1|ID), data=dm) 
summary(EP2POWER_threeway)


#Simple Slopes for H2a.
dm$HighOJS.z <- dm$ObserverJS.z-1
dm$LowOJS.z <- dm$ObserverJS.z+1
dm2$HighOJS.z <- dm2$ObserverJS.z-1
dm2$LowOJS.z <- dm2$ObserverJS.z+1
dm$Ambiguity.c<-revalue(dm$Ambiguity, c('0'='-0.5','1'='0.5'))
dm$Uncertainty.c<-revalue(dm$Uncertainty, c('0'='-0.5','1'='0.5'))
dm2$Ambiguity.c<-revalue(dm2$Ambiguity, c('0'='-0.5','1'='0.5'))
dm2$Uncertainty.c<-revalue(dm2$Uncertainty, c('0'='-0.5','1'='0.5'))

EP2a.high <- lmer(Punishment.x~Ambiguity.c*HighOJS.z + (1|ID), data=dm) 
summary(EP2a.high)
EP2a.low <- lmer(Punishment.x~Ambiguity.c*LowOJS.z + (1|ID), data=dm) 
summary(EP2a.low)

##H2b##

EP2b <- lmer(Punishment.z~Uncertainty*BeneficiaryJS.z + (1|ID), data=dm) 
summary(EP2b)
plot(allEffects(EP2b))

EC2b <- lmer(Compensation.z~Uncertainty*BeneficiaryJS.z + (1|ID), data=dm2) 
summary(EC2b)
plot(allEffects(EC2b))

##H3## PUNISHMENT.
#DV = Behavioral Coding.
EP3_BC <- lm(MAXInterStrength~Punishment.x*Ambiguity+Punishment.x*Uncertainty, data=dm_Intervention)
summary(EP3_BC)
plot(allEffects(EP3_BC))

#DV = Retrospective External Evaluation.
EP3_Retro <- lm(MAXInterStrength_Retro~Punishment.x*Ambiguity+Punishment.x*Uncertainty, data=dm_Intervention)
summary(EP3_Retro)
plot(allEffects(EP3_Retro))

#Effect size only in Ambiguity conditions.
library(dplyr)
dm_Intervention.f <- filter(dm_Intervention,Ambiguity==1)
EP3Ambiguity_BC <- lm(MAXInterStrength~Punishment.x, data=dm_Intervention.f)
summary(EP3Ambiguity_BC)

EP3Ambiguity_Retro <- lm(MAXInterStrength_Retro~Punishment.x, data=dm_Intervention.f)
summary(EP3Ambiguity_Retro)


##H3## COMPENSATION.
#DV = Behavioral Coding.
EC3_BC <- lm(MAXInterStrength~Compensation.x*Ambiguity+Compensation.x*Uncertainty,data=dm2_Intervention)
summary(EC3_BC)

EC3_Retro <- lm(MAXInterStrength_Retro~Compensation.x*Ambiguity+Compensation.x*Uncertainty,data=dm2_Intervention)
summary(EC3_Retro)


###############################################################################################################################

####INFLUENCE OF TYPES OF INTERVENTION IN 3PPG ON EMBEZZLEMENT INTERVENTION###
dm_Intervention[["Row.names"]] <- NULL
dm2_Intervention[["Row.names"]] <- NULL

dm_PC <- merge(dm_Intervention, dm2_Intervention[["Compensation.x"]],by="row.names")

dm_PC$PCDiff <- dm_PC$Punishment.x-dm_PC$y

dm_PC$Type_Intervention <- ifelse(dm_PC[["Punishment.x"]]==0&dm_PC[["y"]]==0,0,
                                     ifelse(dm_PC[["PCDiff"]]==0,1,
                                            ifelse(dm_PC[["PCDiff"]]>0,2,
                                                   ifelse(dm_PC[["PCDiff"]]<0,3,NA))))
dm_PC$Type_Intervention<-as.factor(dm_PC$Type_Intervention)
library(plyr)
dm_PC$Type_Intervention <- revalue(dm_PC$Type_Intervention, c("0"="No intervention", "1"="P = C","2"="P > C","3"="P < C"))




mEPC <- lm(MAXInterStrength~PCDiff*Ambiguity+PCDiff*Uncertainty, data=dm_PC)
summary(mEPC)
plot(allEffects(mEPC))
mEPC_Types <- glm(InterDich~Type_Intervention, data=dm_PC,family = binomial(link = logit))
summary(mEPC_Types)

plot(allEffects(EP3_BC))


  
  
  
  
  
  
  
  

  #CHECKING CATEGORICAL VARIABLES.
  contrasts(df1$SD01_01)  #Shows how categorical variables are dummyfied by R.

#LOGISTIC REGRESSION ANALYSIS.

model <- glm(Survived ~.,family=binomial(link='logit'),data=data1)
summary(model)  

anova(model, test="Chisq")

#MULTILEVEL LOGISTIC REGRESSION.
https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
  
  #OUTPUT TO TABLE.
  install.packages("texreg")
l=list(model)
library(texreg)
texreg(l, booktabs = TRUE, dcolumn = TRUE,file = "texreg.doc", caption="Logistic Regression Model", caption.above=TRUE)

mean(data1$R1.Comp,na.rm=TRUE)
mean(data1$R2.Comp,na.rm=TRUE)
mean(data1$R3.Comp,na.rm=TRUE)
mean(data1$R4.Comp,na.rm=TRUE)

mean(data1$DP04_02,na.rm=TRUE)
table(data1$DP04_02)

save.image(file = "project environment.RData")
?save
