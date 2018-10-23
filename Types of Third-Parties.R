library(Hmisc)
rcorr(as.matrix(data1[,c("R1","R2","R3","R4","R1.Comp","R2.Comp","R3.Comp","R4.Comp")]))

#### TYPES OF INTERVENTION ####
#0 = No intervention at all.
#1 = Same level of Punishment and Compensation.
#2 = More Punishment than Compensation.
#3 = More Compensation than Punishment.

#Punishment - Compensation Per Round.
data1$R1_Diff <- ifelse(data1$R1==0&data1$R1.Comp==0,-9999,data1$R1-data1$R1.Comp)
data1$R2_Diff <- ifelse(data1$R2==0&data1$R2.Comp==0,-9999,data1$R2-data1$R2.Comp)
data1$R3_Diff <- ifelse(data1$R3==0&data1$R3.Comp==0,-9999,data1$R3-data1$R3.Comp)
data1$R4_Diff <- ifelse(data1$R4==0&data1$R4.Comp==0,-9999,data1$R4-data1$R4.Comp)

library(reshape2)
df_types_round <- melt(data1,
            id.vars=c("ID"), #ID variables - variables to keep but not split apart on.
            measure.vars=c('R1_Diff','R2_Diff','R3_Diff','R4_Diff'), #Categories.
            variable.name= "Round", #Name of categorical variable that defines each within-subject condition.
            value.name="P-C" #Name of DV.
)

df_types$Type_Intervention <- ifelse(df_types[["P-C"]]==-9999,0,
                                     ifelse(df_types[["P-C"]]==0,1,
                                            ifelse(df_types[["P-C"]]>0,2,
                                                   ifelse(df_types[["P-C"]]<0,3,NA))))

library(plyr)
df_types$Round<-revalue(df_types$Round, c("R1_Diff"="R1", "R2_Diff"="R2","R3_Diff"="R3","R4_Diff"="R4"))
df_types$Type_Intervention<-as.factor(df_types$Type_Intervention)
df_types$Type_Intervention<-revalue(df_types$Type_Intervention, c("0"="No intervention", "1"="P = C","2"="P > C","3"="P < C"))

xx<-table(df_types$Type_Intervention,df_types$Round)
xx<-xx*100/nrow(data1)

zz<-melt(xx)
colnames(zz)<-c("Type_Intervention","Round","Freq")
zz

library(ggplot2)
ggplot(zz,aes(x=Type_Intervention,y=Freq,fill=Type_Intervention))+
  geom_bar(stat = 'identity', position=position_dodge(width = 0.9))+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(.~Round)+
  theme_bw()


------------------------------------------------------------------------------------------------------------
#Punishment - Compensation Per Decision per Round.
#Round 1.
data1$R1D0_Diff <- ifelse(data1$DP02_01==0&data1$DP02_02==0,-9999,data1$DP02_01-data1$DP02_02)
data1$R1D1_Diff <- ifelse(data1$DP03_01==0&data1$DP03_02==0,-9999,data1$DP03_01-data1$DP03_02)
data1$R1D2_Diff <- ifelse(data1$DP04_01==0&data1$DP04_02==0,-9999,data1$DP04_01-data1$DP04_02)
data1$R1D3_Diff <- ifelse(data1$DP05_01==0&data1$DP05_02==0,-9999,data1$DP05_01-data1$DP05_02)
data1$R1D4_Diff <- ifelse(data1$DP06_01==0&data1$DP06_02==0,-9999,data1$DP06_01-data1$DP06_02)
data1$R1D5_Diff <- ifelse(data1$DP07_01==0&data1$DP07_02==0,-9999,data1$DP07_01-data1$DP07_02)
data1$R1D6_Diff <- ifelse(data1$DP08_01==0&data1$DP08_02==0,-9999,data1$DP08_01-data1$DP08_02)
#Round 2.
data1$R2D0_Diff <- ifelse(data1$DP10_01==0&data1$DP10_02==0,-9999,data1$DP10_01-data1$DP10_02)
data1$R2D1_Diff <- ifelse(data1$DP11_01==0&data1$DP11_02==0,-9999,data1$DP11_01-data1$DP11_02)
data1$R2D2_Diff <- ifelse(data1$DP12_01==0&data1$DP12_02==0,-9999,data1$DP12_01-data1$DP12_02)
data1$R2D3_Diff <- ifelse(data1$DP13_01==0&data1$DP13_02==0,-9999,data1$DP13_01-data1$DP13_02)
data1$R2D4_Diff <- ifelse(data1$DP14_01==0&data1$DP14_02==0,-9999,data1$DP14_01-data1$DP14_02)
data1$R2D5_Diff <- ifelse(data1$DP15_01==0&data1$DP15_02==0,-9999,data1$DP15_01-data1$DP15_02)
data1$R2D6_Diff <- ifelse(data1$DP16_01==0&data1$DP16_02==0,-9999,data1$DP16_01-data1$DP16_02)
#Round 3.
data1$R3D0_Diff <- ifelse(data1$DP18_01==0&data1$DP18_02==0,-9999,data1$DP18_01-data1$DP18_02)
data1$R3D1_Diff <- ifelse(data1$DP19_01==0&data1$DP19_02==0,-9999,data1$DP19_01-data1$DP19_02)
data1$R3D2_Diff <- ifelse(data1$DP20_01==0&data1$DP20_02==0,-9999,data1$DP20_01-data1$DP20_02)
data1$R3D3_Diff <- ifelse(data1$DP21_01==0&data1$DP21_02==0,-9999,data1$DP21_01-data1$DP21_02)
data1$R3D4_Diff <- ifelse(data1$DP22_01==0&data1$DP22_02==0,-9999,data1$DP22_01-data1$DP22_02)
data1$R3D5_Diff <- ifelse(data1$DP23_01==0&data1$DP23_02==0,-9999,data1$DP23_01-data1$DP23_02)
data1$R3D6_Diff <- ifelse(data1$DP24_01==0&data1$DP24_02==0,-9999,data1$DP24_01-data1$DP24_02)
#Round 4.
data1$R4D0_Diff <- ifelse(data1$DP26_01==0&data1$DP26_02==0,-9999,data1$DP26_01-data1$DP26_02)
data1$R4D1_Diff <- ifelse(data1$DP27_01==0&data1$DP27_02==0,-9999,data1$DP27_01-data1$DP27_02)
data1$R4D2_Diff <- ifelse(data1$DP28_01==0&data1$DP28_02==0,-9999,data1$DP28_01-data1$DP28_02)
data1$R4D3_Diff <- ifelse(data1$DP29_01==0&data1$DP29_02==0,-9999,data1$DP29_01-data1$DP29_02)
data1$R4D4_Diff <- ifelse(data1$DP30_01==0&data1$DP30_02==0,-9999,data1$DP30_01-data1$DP30_02)
data1$R4D5_Diff <- ifelse(data1$DP31_01==0&data1$DP31_02==0,-9999,data1$DP31_01-data1$DP31_02)
data1$R4D6_Diff <- ifelse(data1$DP32_01==0&data1$DP32_02==0,-9999,data1$DP32_01-data1$DP32_02)


df_types_decisions <- melt(data1,
                           id.vars=c("ID"), #ID variables - variables to keep but not split apart on.
                           measure.vars=c('R1D0_Diff','R1D1_Diff','R1D2_Diff','R1D3_Diff','R1D4_Diff','R1D5_Diff','R1D6_Diff',
                                          'R2D0_Diff','R2D1_Diff','R2D2_Diff','R2D3_Diff','R2D4_Diff','R2D5_Diff','R2D6_Diff',
                                          'R3D0_Diff','R3D1_Diff','R3D2_Diff','R3D3_Diff','R3D4_Diff','R3D5_Diff','R3D6_Diff',
                                          'R4D0_Diff','R4D1_Diff','R4D2_Diff','R4D3_Diff','R4D4_Diff','R4D5_Diff','R4D6_Diff'), #Categories.
                           variable.name= "Decisions", #Name of categorical variable that defines each within-subject condition.
                           value.name="P-C" #Name of DV.
)
head(df_types_decisions)
df_types_decisions$Type_Intervention <- ifelse(df_types_decisions[["P-C"]]==-9999,0,
                                     ifelse(df_types_decisions[["P-C"]]==0,1,
                                            ifelse(df_types_decisions[["P-C"]]>0,2,
                                                   ifelse(df_types_decisions[["P-C"]]<0,3,NA))))

df_types_decisions$Type_Intervention<-as.factor(df_types_decisions$Type_Intervention)
df_types_decisions$Type_Intervention<-revalue(df_types_decisions$Type_Intervention, c("0"="No intervention", "1"="P = C","2"="P > C","3"="P < C"))

xx2<-table(df_types_decisions$Type_Intervention,df_types_decisions$Decisions)
xx2<-xx2*100/nrow(data1)
xx2
zz2<-melt(xx2)
colnames(zz2)<-c("Type_Intervention","Decisions","Freq")
zz2$Round<-zz2$Decisions

library(plyr)
zz2$Round<-revalue(zz2$Round, c(
  'R1D0_Diff'='Round 1','R1D1_Diff'='Round 1','R1D2_Diff'='Round 1','R1D3_Diff'='Round 1','R1D4_Diff'='Round 1','R1D5_Diff'='Round 1','R1D6_Diff'='Round 1',
  'R2D0_Diff'='Round 2','R2D1_Diff'='Round 2','R2D2_Diff'='Round 2','R2D3_Diff'='Round 2','R2D4_Diff'='Round 2','R2D5_Diff'='Round 2','R2D6_Diff'='Round 2',
  'R3D0_Diff'='Round 3','R3D1_Diff'='Round 3','R3D2_Diff'='Round 3','R3D3_Diff'='Round 3','R3D4_Diff'='Round 3','R3D5_Diff'='Round 3','R3D6_Diff'='Round 3',
  'R4D0_Diff'='Round 4','R4D1_Diff'='Round 4','R4D2_Diff'='Round 4','R4D3_Diff'='Round 4','R4D4_Diff'='Round 4','R4D5_Diff'='Round 4','R4D6_Diff'='Round 4'
))

zz2_round1 <- filter(zz2,Round=="Round 1")
zz2_round2 <- filter(zz2,Round=="Round 2")
zz2_round3 <- filter(zz2,Round=="Round 3")
zz2_round4 <- filter(zz2,Round=="Round 4")

library(ggplot2)
plot_round1<- ggplot(zz2_round1,aes(x=Decisions,y=Freq,fill=Type_Intervention))+
  geom_bar(stat = 'identity', position='stack')+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous("Frequency %",limits=c(0,100))+
  coord_cartesian(ylim=c(4.5,95.5))+
  guides(fill=FALSE)+
  theme_bw()

plot_round2<- ggplot(zz2_round2,aes(x=Decisions,y=Freq,fill=Type_Intervention))+
  geom_bar(stat = 'identity', position='stack')+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous("",limits=c(0,100))+
  coord_cartesian(ylim=c(4.5,95.5))+
  guides(fill=FALSE)+
  theme_bw()

plot_round3<- ggplot(zz2_round3,aes(x=Decisions,y=Freq,fill=Type_Intervention))+
  geom_bar(stat = 'identity', position='stack')+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous("",limits=c(0,100))+
  coord_cartesian(ylim=c(4.5,95.5))+
  guides(fill=FALSE)+
  theme_bw()

plot_round4<- ggplot(zz2_round4,aes(x=Decisions,y=Freq,fill=Type_Intervention))+
  geom_bar(stat = 'identity', position='stack')+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous("",limits=c(0,100))+
  coord_cartesian(ylim=c(4.5,95.5))+
  theme_bw()

library(cowplot)
plot_grid(plot_round1,plot_round2,plot_round3,plot_round4,
          labels = c("R1: Baseline", "R2: Uncertainty", "R3: Ambiguity","R4: Ambiguity + Uncertainty"), ncol=4)
