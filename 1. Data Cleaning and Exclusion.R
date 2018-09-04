################################################################################################################################
##############################################         EXCLUSION CRITERIA       ################################################
################################################################################################################################

rawdata.relabel <- read.csv("Data_Study2_T1T2_31_08_2018.csv",header=TRUE,sep=";",dec=",",na.strings=c("NA","","-9"),row.names=NULL)


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
table(is.na(rawdata.relabel$Excl1)) #4 people has NAs on DQ01 or DQ02.

##Duration and concentration items.
rawdata.relabel$TIME_SUM_T1<-rawdata.relabel$TIME_SUM.x/60
table(rawdata.relabel$TIME_SUM_T1<45)
table(rawdata.relabel$KO01_01)
table(rawdata.relabel$KO02_01)

rawdata.relabel$Excl2<-ifelse(rawdata.relabel$TIME_SUM_T1<45
                              &(rawdata.relabel$KO01_01!=5|rawdata.relabel$KO02_01!=1),1,0) 
table(rawdata.relabel$Excl2) #0 people are excluded based on Duration and Concentration Items.
table(is.na(rawdata.relabel$Excl2))  #2 people had low times, and NAs in Concentration Items.


##Delete Excluded cases. Total N = 163.
library(dplyr)
rawdata.excl <- filter(rawdata.relabel, Excl1!=1 & !is.na(Excl1)) %>% filter(Excl2==0)

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

write.csv2(rawdata.excl, file = "Data Study2 T1&T2 (Analyses file).csv")


