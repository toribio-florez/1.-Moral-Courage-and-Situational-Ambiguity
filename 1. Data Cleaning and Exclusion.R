################################################################################################################################
##############################################         EXCLUSION CRITERIA       ################################################
################################################################################################################################

rawdata.relabel <- read.csv("Data_Study2_T1T2_31_08_2018.csv",header=TRUE,sep=";",dec=",",na.strings=c("NA","","-9"),row.names=NULL)
colnames(rawdata.relabel)[colnames(rawdata.relabel)=="ï..TN"] <- "TN"
  
#If it is not specified that DECIMALS are indicated by ",", R will identify variables with commas as FACTORS or CHARACTERS.
#For transforming from FACTOR to NUMERIC:
for(i in c(ColumnA:ColumnX)) {                                
  rawdata.relabel[,i] <- as.numeric(as.character(rawdata.relabel[,i]))
}
#IMPORTANT:Sometimes R messes up the values during this transformation. Check whether the frequencies table change.


##Self-reported data quality.
#NOTE: In Exclusion Criteria doc, scale has 6 points. In Soscisurvey, scale goes from 1 to 5.
table(rawdata.relabel$DQ01)
table(rawdata.relabel$DQ02)
rawdata.relabel$Excl1<-ifelse(rawdata.relabel$DQ01&rawdata.relabel$DQ02<=2,1,0) 
table(rawdata.relabel$Excl1) #2 people are excluded based on self-report data quality.
table(is.na(rawdata.relabel$Excl1)) #10 NAs, 4 of them with NAs on DQ01 or DQ02.

##Duration and concentration items.
rawdata.relabel$TIME_SUM_T1<-rawdata.relabel$TIME_SUM.x/60
table(rawdata.relabel$TIME_SUM_T1<45)
table(rawdata.relabel$KO01_01)
table(rawdata.relabel$KO02_01)

rawdata.relabel$Excl2<-ifelse(rawdata.relabel$TIME_SUM_T1<45
                              &(rawdata.relabel$KO01_01!=5|rawdata.relabel$KO02_01!=1),1,0) 

table(rawdata.relabel$Excl2) #0 people are excluded based on Duration and Concentration Items.
table(is.na(rawdata.relabel$Excl2))  #6 NAs, and 2 people had low times and NAs in Concentration Items.
table(is.na(rawdata.relabel$Excl1)&is.na(rawdata.relabel$Excl2)) #All the NAs from Excl2 are also NAs for Excl1.

#EXCLUDED CASES AFTER EXCLUSION AND NAs.
#From 176 rows, we delete 2 excluded based on general exclusion criteria and delete 10 NAs.
#Subset for 3PPG Analyses = 164 observations.

##3PPG Control items in each Round.
#Baseline. Correct Answers: R101 (7) - R102 (2)
R101 <- table(rawdata.relabel$DP48_01)
R102 <- table(rawdata.relabel$DP48_02)
R101
R102
#High Uncertainty/Low Ambiguity. Correct Answers: R201 (7) - R202 (10)
R201 <- table(rawdata.relabel$DP49_01)
R202 <- table(rawdata.relabel$DP49_02)
R201
R202
#Low Uncertainty/High Ambiguity. Correct Answers: R301 (10) - R302 (2)
R301 <- table(rawdata.relabel$DP50_01)
R302 <- table(rawdata.relabel$DP50_02)
R301
R302
#High Uncertainty/High Ambiguity. Correct Answers: R301 (10) - R302 (10)
R401 <- table(rawdata.relabel$DP51_01)
R402 <- table(rawdata.relabel$DP51_02)
R401
R402
#In total AROUND 25 people failed 3PPG attention checks, but were redirected to instructions.


################################################################################################################################
##############################################    CREATION OF DATA SUBSET      #################################################
################################################################################################################################

#Data subset applying GENERAL EXCLUSION CRITERIA: Total N = 164.
library(dplyr)
data_base <- filter(rawdata.relabel, Excl1==0) %>% filter(Excl2==0) %>% select(
         TN,
         SD01_01,SD01_02,SD01_03,SD01_03a,SD01_04, #Sociodemographics.
         DQ01,DQ02,  #Control Careless response.
         KO01_01,KO02_01,  #Attention checks.
         JS01_01,JS01_02,  #Victim JS.
         JS02_01,JS02_02,  #Observer JS.
         JS03_01,JS03_02,  #Beneficiary JS.
         JS04_01,JS04_02,  #Perpetrator JS.
         PF01_12,PF01_13,PF01_14,PF01_15, #Fear of Invalidity.
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
data1 <- data_base

write.csv2(data1, file = "Data Study2 T1&T2 (Analyses File).csv")

