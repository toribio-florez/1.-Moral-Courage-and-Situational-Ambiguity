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
data1$VictimJS.z <- scale(data1$VictimJS)[,]  #Standardized Victim JS.
data1$ObserverJS <- rowMeans(data1[c("JS02_01","JS02_02")],na.rm=TRUE)  #Observer JS.
data1$ObserverJS.z <- scale(data1$ObserverJS)[,] #Standardized Observer JS.
data1$BeneficiaryJS <- rowMeans(data1[c("JS03_01","JS03_02")],na.rm=TRUE) #Beneficiary JS.
data1$BeneficiaryJS.z <- scale(data1$BeneficiaryJS)[,] #Standardized Beneficiary JS.
data1$PerpetratorJS <- rowMeans(data1[c("JS04_01","JS04_02")],na.rm=TRUE)  #Perpetrator JS.
data1$PerpetratorJS.z <- scale(data1$PerpetratorJS)[,]  #Perpetrator JS.

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
data1$R2<-rowSums(data1[,c('DP10_01','DP11_01','DP12_01','DP13_01','DP14_01')],na.rm = TRUE)
data1$R3<-rowSums(data1[,c('DP18_01','DP19_01','DP20_01','DP21_01','DP22_01')],na.rm = TRUE)
data1$R4<-rowSums(data1[,c('DP26_01','DP27_01','DP28_01','DP29_01','DP30_01')],na.rm = TRUE)

data1$R1.Comp<-rowSums(data1[,c('DP02_02','DP03_02','DP04_02','DP05_02','DP06_02')],na.rm = TRUE)
data1$R2.Comp<-rowSums(data1[,c('DP10_02','DP11_02','DP12_02','DP13_02','DP14_02')],na.rm = TRUE)
data1$R3.Comp<-rowSums(data1[,c('DP18_02','DP19_02','DP20_02','DP21_02','DP22_02')],na.rm = TRUE)
data1$R4.Comp<-rowSums(data1[,c('DP26_02','DP27_02','DP28_02','DP29_02','DP30_02')],na.rm = TRUE)

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
data1$Punishment.R1<-ifelse(data1$R1>0,1,0)
data1$Punishment.R2<-ifelse(data1$R2>0,1,0)
data1$Punishment.R3<-ifelse(data1$R3>0,1,0)
data1$Punishment.R4<-ifelse(data1$R4>0,1,0)

table(data1$Punishment.R1) #Frequencies.
table(data1$Punishment.R2)
table(data1$Punishment.R3)
table(data1$Punishment.R4)

##############################################  DATA RESHAPE: WIDE TO LONG FORMAT  ################################################
#Reshape from wide to long format. Individually for each Categorical Variable.
############PUNISHMENT DICHOTOMOUS##########
library(reshape2)
dich1 <- melt(data1,
              id.vars=c("SD01_01","SD01_02","SD01_03","SD01_04",
                        "ObserverJS.z","BeneficiaryJS.z","PerpetratorJS.z"), #ID variables - variables to keep but not split apart on.
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
                      "ObserverJS.z","BeneficiaryJS.z","PerpetratorJS.z"), #ID variables - variables to keep but not split apart on.
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
                      "ObserverJS.z","BeneficiaryJS.z","PerpetratorJS.z"), #ID variables - variables to keep but not split apart on.
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

save.image(file = "project environment.RData")
?save
