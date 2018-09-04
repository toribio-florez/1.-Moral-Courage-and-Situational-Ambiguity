options(scipen=99)    #Scientific Notation to fixed instead of exponential.

################################################################################################################################
############################################## DATA SHAPING & COMPUTATION OF VARIABLES #########################################
################################################################################################################################
#Creating subset with variables of interest.
library(dplyr)
data1<-  select(rawdata.excl, 
                SD01_01,SD01_02,SD01_03,SD01_03a,SD01_04, #Sociodemographics.
                DQ01,DQ02,  #Control Careless response.
                KO01_01,KO02_01,  #Attention checks.
                JS01_01,JS01_02,  #Victim JS.
                JS02_01,JS02_02,  #Observer JS.
                JS03_01,JS03_02,  #Beneficiary JS.
                JS04_01,JS04_02,  #Perpetrator JS.
                DP00_01,DP00_02,  #3GGP Instructions Comprehension checks.
                DP01_01,DP02_01,DP02_02,DP03_01,DP03_02,DP04_01,DP04_02,DP05_01,DP05_02,DP06_01,DP06_02,DP07_01,DP07_02,DP08_01,DP08_02,  #3PPG Round 1.
                DP09_01,DP10_01,DP10_02,DP11_01,DP11_02,DP12_01,DP12_02,DP13_01,DP13_02,DP14_01,DP14_02,DP15_01,DP15_02,DP16_01,DP16_02,  #3PPG Round 2.
                DP17_01,DP18_01,DP18_02,DP19_01,DP19_02,DP20_01,DP20_02,DP21_01,DP21_02,DP22_01,DP22_02,DP23_01,DP23_02,DP24_01,DP24_02,  #3PPG Round 3.
                DP25_01,DP26_01,DP26_02,DP27_01,DP27_02,DP28_01,DP28_02,DP29_01,DP29_02,DP30_01,DP30_02,DP31_01,DP31_02,DP32_01,DP32_02,  #3PPG Round 4.
                DP48_01,DP48_02,DP49_01,DP49_02,DP50_01,DP50_02,DP51_01,DP51_02, #Rounds Comprehension Checks.
                Exclusion_doubts, #Exclusion Decision based on Ratings of Reactions to the Embezzlement.
                D_Intervention_4, IG_intervention_4,
                D_Intervention_5a, IG_intervention_5a, 
                D_Intervention_5b, IG_intervention_5b, 
                D_Intervention_6a, IG_intervention_6a, 
                D_Intervention_6b, IG_intervention_6b, 
                D_Intervention_6c, IG_intervention_6c, 
                D_Intervention_6d, IG_intervention_6d, 
                D_Intervention_8, IG_intervention_8,
                RF01_00,RF01_01, #Retrospective Intervention EXPERIMENTER 1.
                RF01_A_06,RF01_A_07,RF01_A_08,
                RF01_B_06,RF01_B_07,RF01_B_08,
                RF01_C_06,RF01_C_07,RF01_C_08,
                RF01_D_06,RF01_D_07,RF01_D_08,
                RF02_01, #Retrospective Intervention EXPERIMENTER 2.
                RF02_A_06,RF02_A_07,RF02_A_08,
                RF03_01, #Retrospective Intervention CONFEDERATE.
                RF03_A_06,RF03_A_07,RF03_A_08,
                RF03_B_06,RF03_B_07,RF03_B_08,
                RF03_C_06,RF03_C_07,RF03_C_08,
                RF03_D_06,RF03_D_07,RF03_D_08,
                RF03_E_06,RF03_E_07,RF03_E_08,
                RF04_01, #Retrospective Intervention PROJECT LEADER.
                RF04_A_06,RF04_A_07,RF04_A_08,
                TIME_SUM_T1,   #Experiment Duration.
                KO01_01,KO02_01     #Concentration Items.
)

#Creating ID column. Our CASE variable (CASE_T2) has multiple NAs.
data1$ID <- seq.int(nrow(data1))

##############################################   JUSTICE SENSITIVITY   ################################################

#Calculating and Standardizing aggregated scores of JS.
library(psych)
data1$VictimJS <- rowMeans(data1[c("JS01_01","JS01_02")],na.rm=TRUE)  #Victim JS.
data1$VictimJS.z <- scale(data1$VictimJS)[,]  #Standardized Victim JS.
data1$ObserverJS <- rowMeans(data1[c("JS02_01","JS02_02")],na.rm=TRUE)  #Observer JS.
data1$ObserverJS.z <- scale(ObserverJS)[,] #Standardized Observer JS.
data1$BeneficiaryJS <- rowMeans(data1[c("JS03_01","JS03_02")],na.rm=TRUE) #Beneficiary JS.
data1$BeneficiaryJS.z <- scale(BeneficiaryJS)[,] #Standardized Beneficiary JS.
data1$PerpetratorJS <- rowMeans(data1[c("JS04_01","JS04_02")],na.rm=TRUE)  #Perpetrator JS.
data1$PerpetratorJS.z <- scale(PerpetratorJS)[,]  #Perpetrator JS.

OJSm <- mean(data1$ObserverJS,na.rm=TRUE)
OJSsd <- sd(data1$ObserverJS,na.rm=TRUE)
PJSm <- mean(data1$PerpetratorJS,na.rm=TRUE)
PJSsd <- sd(data1$PerpetratorJS,na.rm=TRUE)
BJSm <- mean(data1$BeneficiaryJS,na.rm=TRUE)
BJSsd <- sd(data1$BeneficiaryJS,na.rm=TRUE)

##############################################   3PPG: PUNISHMENT/COMPENSATION   ################################################
#CONTINUOUS.
#Calculate Total PUNISHMENT/COMPENSATION in each Round across decisions.
data1$R1<-data1$DP02_01+data1$DP03_01+data1$DP04_01+data1$DP05_01+data1$DP06_01
data1$R2<-data1$DP10_01+data1$DP11_01+data1$DP12_01+data1$DP13_01+data1$DP14_01
data1$R3<-data1$DP18_01+data1$DP19_01+data1$DP20_01+data1$DP21_01+data1$DP22_01
data1$R4<-data1$DP26_01+data1$DP27_01+data1$DP28_01+data1$DP29_01+data1$DP30_01

data1$R1b<-data1$DP02_01+data1$DP03_01+data1$DP04_01+data1$DP05_01+data1$DP06_01+data1$DP07_01+data1$DP08_01 
data1$R2b<-data1$DP10_01+data1$DP11_01+data1$DP12_01+data1$DP13_01+data1$DP14_01+data1$DP15_01+data1$DP16_01
data1$R3b<-data1$DP18_01+data1$DP19_01+data1$DP20_01+data1$DP21_01+data1$DP22_01+data1$DP23_01+data1$DP24_01
data1$R4b<-data1$DP26_01+data1$DP27_01+data1$DP28_01+data1$DP29_01+data1$DP30_01+data1$DP31_01+data1$DP32_01

data1$R1.Comp<-data1$DP02_02+data1$DP03_02+data1$DP04_02+data1$DP05_02+data1$DP06_02
data1$R2.Comp<-data1$DP10_02+data1$DP11_02+data1$DP12_02+data1$DP13_02+data1$DP14_02
data1$R3.Comp<-data1$DP18_02+data1$DP19_02+data1$DP20_02+data1$DP21_02+data1$DP22_02
data1$R4.Comp<-data1$DP26_02+data1$DP27_02+data1$DP28_02+data1$DP29_02+data1$DP30_02

data1$R1b.Comp<-data1$DP02_02+data1$DP03_02+data1$DP04_02+data1$DP05_02+data1$DP06_02+data1$DP07_02+data1$DP08_02 
data1$R2b.Comp<-data1$DP10_02+data1$DP11_02+data1$DP12_02+data1$DP13_02+data1$DP14_02+data1$DP15_02+data1$DP16_02
data1$R3b.Comp<-data1$DP18_02+data1$DP19_02+data1$DP20_02+data1$DP21_02+data1$DP22_02+data1$DP23_02+data1$DP24_02
data1$R4b.Comp<-data1$DP26_02+data1$DP27_02+data1$DP28_02+data1$DP29_02+data1$DP30_02+data1$DP31_02+data1$DP32_02

#PUNISHMENT: DICHOTOMOUS.
data1$Punishment.R1<-ifelse(data1$R1>0,1,0)
data1$Punishment.R2<-ifelse(data1$R2>0,1,0)
data1$Punishment.R3<-ifelse(data1$R3>0,1,0)
data1$Punishment.R4<-ifelse(data1$R4>0,1,0)

table(data1$Punishment.R1) #Frequencies.
table(data1$Punishment.R2)
table(data1$Punishment.R3)
table(data1$Punishment.R4)

##############################################  INTERVENTION BEHAVIOR (BEHAVIORAL CODING) ################################################
#CONTINUOUS.
#Inter-rater Reliability.
intervCol <- grep("ntervention",colnames(data1),value=FALSE)   #Column numbers of Intervention Ratings for every Embezzlement stage.

library(psych)
IRR_4 <- alpha(as.matrix(data1[,intervCol[1]:intervCol[2]]))    #Interrater Reliability for Phase 4.
IRR_4 #alpha = .91.
IRR_5a <- alpha(as.matrix(data1[,intervCol[3]:intervCol[4]]))    #Interrater Reliability for Phase 5a.
IRR_5a #alpha = .94.
IRR_5b <- alpha(as.matrix(data1[,intervCol[5]:intervCol[6]]))    #Interrater Reliability for Phase 5b.
IRR_5b #No variance on Rater 1 (D).
IRR_6a <- alpha(as.matrix(data1[,intervCol[7]:intervCol[8]]))    #Interrater Reliability for Phase 6a.
IRR_6a #alpha = .99.
IRR_6b <- alpha(as.matrix(data1[,intervCol[9]:intervCol[10]]))    #Interrater Reliability for Phase 6b.
IRR_6b #alpha = .66.
IRR_6c <- alpha(as.matrix(data1[,intervCol[11]:intervCol[12]]))    #Interrater Reliability for Phase 6c.
IRR_6c #No variance on Rater 1 (D) and Rater 2 (IG).
IRR_6d <- alpha(as.matrix(data1[,intervCol[13]:intervCol[14]]))    #Interrater Reliability for Phase 6d.
IRR_6d #alpha = .83.
IRR_8 <- alpha(as.matrix(data1[,intervCol[15]:intervCol[16]]))    #Interrater Reliability for Phase 8.
IRR_8 #alpha = .49.
        
#Aggregate ratings across raters for each Phase.
data1$InterStrength_4 <- rowMeans(data1[,intervCol[1]:intervCol[2]],na.rm=TRUE)
data1$InterStrength_5a <- rowMeans(data1[,intervCol[3]:intervCol[4]],na.rm=TRUE)
data1$InterStrength_5b <- rowMeans(data1[,intervCol[5]:intervCol[6]],na.rm=TRUE)
data1$InterStrength_6a <- rowMeans(data1[,intervCol[7]:intervCol[8]],na.rm=TRUE)
data1$InterStrength_6b <- rowMeans(data1[,intervCol[9]:intervCol[10]],na.rm=TRUE)
data1$InterStrength_6c <- rowMeans(data1[,intervCol[11]:intervCol[12]],na.rm=TRUE)
data1$InterStrength_6d <- rowMeans(data1[,intervCol[13]:intervCol[14]],na.rm=TRUE)
data1$InterStrength_8 <- rowMeans(data1[,intervCol[15]:intervCol[16]],na.rm=TRUE)

#Create Phase 7 - Project Leader's rating when there was no videorecording.
data1$InterStrength_7_1 <- ifelse(data1$RF04_A_06==1,1,0)
data1$InterStrength_7_2 <- ifelse(data1$RF04_A_07==1,2,0)
data1$InterStrength_7_3 <- ifelse(data1$RF04_A_08==1,3,0)

data1$InterStrength_7 <- rowSums(data1[,grep("InterStrength_7_",colnames(data1),value=FALSE)],na.rm = TRUE)

data1[,grep("InterStrength_7_",colnames(data1),value=FALSE)]<-NULL #Delete intermediate variables.

#Maximum Intervention Strength across Phases.
data1$MAXInterStrength <- apply(data1[,grep("InterStrength_",colnames(data1),value=FALSE)],1,max)

#DICHOTOMOUS.
data1$InterDich <- ifelse(data1$MAXInterStrength==0,0,1)
table(data1$InterDich)


##############################################  INTERVENTION BEHAVIOR (RETROSPECTIVE EXTERNAL EVALUATION)  ################################################
#DICHOTOMOUS.

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

data1$Retro.Sum <- rowSums(data1[c("EX1_A_06", "EX1_A_07", "EX1_A_08",
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

data1$INT_dich.retro <- ifelse(data1$Retro.Sum==0,0,1)
table(data1$INT_dich.retro)

#INTERVENTION: Continuous.
#From dichotomous YES/NO, create continuous variable from 1 to 3, depending on whether people
#1 = make a comment without labeling the situation as wrong (__06), 
#2 = label the situation as wrong, immoral or fraud (_07),
#3 = Stop the fraud, threat to report it (_08).

library(dplyr)
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

data1$INT_con.retro <-  apply(data1[c("EX1_A_06c", "EX1_A_07c", "EX1_A_08c",
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

table(data1$INT_con.retro)

##############################################  DATA RESHAPE: WIDE TO LONG FORMAT  ################################################
#Reshape from wide to long format. Individually for each Categorical Variable.
############PUNISHMENT DICHOTOMOUS##########
library(reshape2)
dich1 <- melt(data1,
              id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                        "ObserverJS.c","BeneficiaryJS.c","PerpetratorJS.c",
                        "INT_dich.retro","INT_con.retro"), #ID variables - variables to keep but not split apart on.
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

############PUNISHMENT CONTINUOUS###########
library(reshape2)
df1 <- melt(data1,
            id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                      "ObserverJS.c","BeneficiaryJS.c","PerpetratorJS.c",
                      "INT_dich.retro","INT_con.retro"), #ID variables - variables to keep but not split apart on.
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

############COMPENSATION CONTINUOUS###########
library(reshape2)
df3 <- melt(data1,
            id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                      "ObserverJS.c","BeneficiaryJS.c","PerpetratorJS.c",
                      "INT_dich.retro","INT_con.retro"), #ID variables - variables to keep but not split apart on.
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

################################################################################################################################
##############################################    DESCRIPTIVES STATISTICS    ################################################
################################################################################################################################

##SOCIODEMOGRAPHICS.
summary(data1$SD01_01)
table(data1$SD01_02)
table(data1$SD01_03)

#Plot for mapping missing values.
install.packages("Amelia")
library(Amelia)
missmap(data1, main = "Missing values vs observed") 

sum(is.na(dm$Punishment.x))   #Number of NAs in new file (N=684).
sapply(data1,function(x) sum(is.na(x)))    #Number of NAs in original file (N=171).


################ DEPENDENT MEASURES ################
#Dichotomous Punishment, probabilities per treatment.
library(plyr)
#Probabilities for Ambiguity Manipulation.
freq1<-count(dich1,c("Ambiguity","Punishment"))
freq1$Prob.<-freq1$freq/sum(dich1$Ambiguity==0)
freq1
#Probabilities for Uncertainty Manipulation.
freq2<-count(dich2,c("Uncertainty","Punishment"))
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
#Bivariate correlations of JS.
library(Hmisc)
which(colnames(data1)=="ObserverJS")
which(colnames(data1)=="BeneficiaryJS")
JScorr <- rcorr(as.matrix(data1[,91:93]))
JScorr

which(colnames(data1)=="R1.Comp")
which(colnames(data1)=="PerpetratorJS")
JScorr2 <- rcorr(as.matrix(data1[,c(128,110,111,112,113)]))
JScorr
JScorr2

#Chronbachs alpha.
which(colnames(data1)=="JS02_01")
which(colnames(data1)=="JS04_02")
library(psych)
alpha(as.matrix(data1[,12:17]))
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


################################################################################################################################
##############################################           MAIN ANALYSIS          ################################################
################################################################################################################################

library(effects)
library(lme4)
library(lmerTest)
library(lattice)
library(sjPlot)



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
H2a<- glmer(Punishment.x~Ambiguity*PerpetratorJS.c+Ambiguity*ObserverJS.c+ (1|ID), data=dichm,family = binomial(link = logit)) 
summary(H2a)
plot(allEffects(H2a))

H2aCIs <- confint(H2a,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aORs <- cbind(Coeff.=fixef(H2a),OR=exp(fixef(H2a)),exp(H2aCIs))
H2aORs

##H2a ## Simple Slope Analysis.
dichm$HighOJS.c <- dichm$ObserverJS.c-OJSsd
dichm$LowOJS.c <- dichm$ObserverJS.c+OJSsd
dichm$Ambiguity.c<-revalue(dichm$Ambiguity, c('0'='-0.5','1'='0.5'))
dichm$Uncertainty.c<-revalue(dichm$Uncertainty, c('0'='-0.5','1'='0.5'))

H2a.high<- glmer(Punishment.x~Ambiguity.c*HighOJS.c+ (1|ID), data=dichm,family = binomial(link = logit))
summary(H2a.high)
H2aHCIs <- confint(H2a.high,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aHORs <- cbind(Coeff.=fixef(H2a.high),OR=exp(fixef(H2a.high)),exp(H2aHCIs))
H2aHORs

H2a.low<- glmer(Punishment.x~Ambiguity.c*LowOJS.c+ (1|ID), data=dichm,family = binomial(link = logit))
summary(H2a.low)
H2aLCIs <- confint(H2a.low,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2aLORs <- cbind(Coeff.=fixef(H2a.low),OR=exp(fixef(H2a.low)),exp(H2aLCIs))
H2aLORs


##H2b##

H2b<- glmer(Punishment.x~Uncertainty*BeneficiaryJS.c+ (1|ID), data=dichm,family = binomial(link = logit)) 
summary(H2b)
plot(allEffects(mod2b))

H2bCIs <- confint(H2b,parm="beta_",level = 0.95) #Calculate 95%CIs for fixed effects b coefficients.
H2bORs <- cbind(Coeff.=fixef(H2b),OR=exp(fixef(H2b)),exp(H2bCIs))
H2bORs


##Plotting Effects from Logistic Regression.
http://data.library.virginia.edu/visualizing-the-effects-of-logistic-regression/
  
  
##H3##
#Exclusion based on Doubts and Detection of the Embezzlement.
library(dplyr)
table(data1$Exclusion_doubts) #32 Participants detected or expressed doubts about the Embezzlement.




H3 <- glmer(INT_dich.retro~Punishment.x*Ambiguity+Punishment.x*Uncertainty + (1|ID), data=dichm,family = binomial(link = logit))
summary(H3)
plot(allEffects(H3))
  



################################################################################################################################
##############################################           EXPLORATORY ANALYSIS          #########################################
################################################################################################################################
library(Amelia)
missmap(data1[19:88], main = "Missing values vs observed") 
summary(data1[95:98])
summary(data1[99:102])


##H1##

library(ggplot2)
EP1 <- lmer(Punishment.x~Ambiguity+Uncertainty + (1|ID), data=dm) 
summary(EP1)
plot(allEffects(EP1))

TEST <- lmer(Punishment.x~Ambiguity+Uncertainty + (1+Ambiguity|ID), data=dm)
summary(TEST)

EC1 <- lmer(Compensation.x~Ambiguity+Uncertainty + (1|ID), data=dm2) 
summary(EC1)





#Simple effects.
library(phia)
testInteractions(EC1, pairwise="Ambiguity", fixed="Uncertainty",adjustment="none")
testInteractions(EC1, pairwise="Uncertainty", fixed="Ambiguity",adjustment="none")



##H2a##

EP2a <- lmer(Punishment.x~Ambiguity*PerpetratorJS.c+Ambiguity*ObserverJS.c + (1|ID), data=dm) 
summary(EP2a)
plot(allEffects(EP2a))

EP2POWER <- lmer(Punishment.x~Ambiguity*ObserverJS.c + (1|ID), data=dm) 
summary(EP2POWER)
?var
?chol

EC2a <- lmer(Compensation.x~Ambiguity*PerpetratorJS.c+Ambiguity*ObserverJS.c + (1|ID), data=dm2) 
summary(EC2a)
plot(allEffects(EC2a))

#Simple Slopes for H2a.
dm$HighOJS.c <- dm$ObserverJS.c-sd1
dm$LowOJS.c <- dm$ObserverJS.c+sd1
dm2$HighOJS.c <- dm2$ObserverJS.c-sd1
dm2$LowOJS.c <- dm2$ObserverJS.c+sd1
dm$Ambiguity.c<-revalue(dm$Ambiguity, c('0'='-0.5','1'='0.5'))
dm$Uncertainty.c<-revalue(dm$Uncertainty, c('0'='-0.5','1'='0.5'))
dm2$Ambiguity.c<-revalue(dm2$Ambiguity, c('0'='-0.5','1'='0.5'))
dm2$Uncertainty.c<-revalue(dm2$Uncertainty, c('0'='-0.5','1'='0.5'))

EP2a.high <- lmer(Punishment.x~Ambiguity.c*HighOJS.c + (1|ID), data=dm) 
summary(EP2a.high)
EP2a.low <- lmer(Punishment.x~Ambiguity.c*LowOJS.c + (1|ID), data=dm) 
summary(EP2a.low)

##H2b##

EP2b <- lmer(Punishment.x~Uncertainty*BeneficiaryJS.c + (1|ID), data=dm) 
summary(EP2b)
plot(allEffects(EP2b))

EC2b <- lmer(Compensation.x~Uncertainty*BeneficiaryJS.c + (1|ID), data=dm2) 
summary(EC2b)
plot(allEffects(EC2b))


##H3##
library(dplyr)
EP3 <- lm(INT_con.retro~Punishment.x*Ambiguity+Punishment.x*Uncertainty, data=dm)
summary(EP3)
plot(allEffects(EP3))

#Effect size only in Ambiguity conditions.
library(dplyr)
dm.f <- filter(dm,Ambiguity==1)
EP3.f <- lm(INT_con.retro~Punishment.x, data=dm.f)
summary(EP3.f)
plot(allEffects(EP3.f))


EC3 <- lm(INT_con.retro~Compensation.x*Ambiguity+Compensation.x*Uncertainty + (1|ID),data=dm2)
summary(EC3)
plot(allEffects(EC3))


------------------------------------------------------------------------------------------------------------------------
  
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
