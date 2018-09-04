################################################################################################################################
##############################################         STRATEGY METHOD          ################################################
################################################################################################################################

##PLOTTING DECISION PATTERN ACROSS ROUNDS.
#PUNISHMENT#
library(reshape2)
library(plyr)

smp <- melt(data1,
            id.vars='ID', #ID variables - variables to keep but not split apart on.
            measure.vars=c('DP02_01','DP03_01','DP04_01','DP05_01','DP06_01','DP07_01','DP08_01',
                           'DP10_01','DP11_01','DP12_01','DP13_01','DP14_01','DP15_01','DP16_01',
                           'DP18_01','DP19_01','DP20_01','DP21_01','DP22_01','DP23_01','DP24_01',
                           'DP26_01','DP27_01','DP28_01','DP29_01','DP30_01','DP31_01','DP32_01'), #Categories.
            variable.name= "Decisions", #Name of categorical variable that defines each within-subject condition.
            value.name="Punishment" #Name of DV.
)

library(plyr)
C1<-ddply(smp, .(Decisions), summarise,
          N    = length(Punishment),
          mean = mean(Punishment, na.rm=TRUE),
          sd   = sd(Punishment,na.rm = TRUE),
          se   = sd / sqrt(N))

C1$Rounds = C1$Decisions
C1$Rounds <- revalue(C1$Rounds, c('DP02_01'='Round 1','DP03_01'='Round 1','DP04_01'='Round 1','DP05_01'='Round 1','DP06_01'='Round 1','DP07_01'='Round 1','DP08_01'='Round 1',
                                  'DP10_01'='Round 2','DP11_01'='Round 2','DP12_01'='Round 2','DP13_01'='Round 2','DP14_01'='Round 2','DP15_01'='Round 2','DP16_01'='Round 2',
                                  'DP18_01'='Round 3','DP19_01'='Round 3','DP20_01'='Round 3','DP21_01'='Round 3','DP22_01'='Round 3','DP23_01'='Round 3','DP24_01'='Round 3',
                                  'DP26_01'='Round 4','DP27_01'='Round 4','DP28_01'='Round 4','DP29_01'='Round 4','DP30_01'='Round 4','DP31_01'='Round 4','DP32_01'='Round 4'))
C1$Decisions <- revalue(C1$Decisions, c('DP02_01'=0,'DP03_01'=1,'DP04_01'=2,'DP05_01'=3,'DP06_01'=4,'DP07_01'=5,'DP08_01'=6,
                                        'DP10_01'=0,'DP11_01'=1,'DP12_01'=2,'DP13_01'=3,'DP14_01'=4,'DP15_01'=5,'DP16_01'=6,
                                        'DP18_01'=0,'DP19_01'=1,'DP20_01'=2,'DP21_01'=3,'DP22_01'=4,'DP23_01'=5,'DP24_01'=6,
                                        'DP26_01'=0,'DP27_01'=1,'DP28_01'=2,'DP29_01'=3,'DP30_01'=4,'DP31_01'=5,'DP32_01'=6))

library(ggplot2)
library(RColorBrewer)
p1 <-ggplot(data=C1, aes(x=Decisions, y=mean, fill=Decisions))+
  facet_grid(.~Rounds)+
  geom_bar(position=position_dodge(),colour="black", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+geom_line()+
  guides(fill=FALSE)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=16),
                   axis.title.x=element_text(face="bold", size=14),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   strip.text.x = element_text(size = 12))+
  labs(x = "Person A Gives",
       y = "Punishment Intensity")+
  scale_x_discrete(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6,7,8,9,10),limits=c(0,7))+
  scale_fill_brewer(palette='Reds',direction=-1)
print(p1)



#COMPENSATION#
smc <- melt(data1,
            id.vars='ID', 
            measure.vars=c('DP02_02','DP03_02','DP04_02','DP05_02','DP06_02','DP07_02','DP08_02',
                           'DP10_02','DP11_02','DP12_02','DP13_02','DP14_02','DP15_02','DP16_02',
                           'DP18_02','DP19_02','DP20_02','DP21_02','DP22_02','DP23_02','DP24_02',
                           'DP26_02','DP27_02','DP28_02','DP29_02','DP30_02','DP31_02','DP32_02'), 
            variable.name= "Decisions", 
            value.name="Compensation"
)


C2<-ddply(smc, .(Decisions), summarise,
          N    = length(Compensation),
          mean = mean(Compensation, na.rm=TRUE),
          sd   = sd(Compensation,na.rm = TRUE),
          se   = sd / sqrt(N))

C2$Rounds = C2$Decisions
C2$Rounds <- revalue(C2$Rounds, c('DP02_02'='Round 1','DP03_02'='Round 1','DP04_02'='Round 1','DP05_02'='Round 1','DP06_02'='Round 1','DP07_02'='Round 1','DP08_02'='Round 1',
                                  'DP10_02'='Round 2','DP11_02'='Round 2','DP12_02'='Round 2','DP13_02'='Round 2','DP14_02'='Round 2','DP15_02'='Round 2','DP16_02'='Round 2',
                                  'DP18_02'='Round 3','DP19_02'='Round 3','DP20_02'='Round 3','DP21_02'='Round 3','DP22_02'='Round 3','DP23_02'='Round 3','DP24_02'='Round 3',
                                  'DP26_02'='Round 4','DP27_02'='Round 4','DP28_02'='Round 4','DP29_02'='Round 4','DP30_02'='Round 4','DP31_02'='Round 4','DP32_02'='Round 4'))
C2$Decisions <- revalue(C2$Decisions, c('DP02_02'=0,'DP03_02'=1,'DP04_02'=2,'DP05_02'=3,'DP06_02'=4,'DP07_02'=5,'DP08_02'=6,
                                        'DP10_02'=0,'DP11_02'=1,'DP12_02'=2,'DP13_02'=3,'DP14_02'=4,'DP15_02'=5,'DP16_02'=6,
                                        'DP18_02'=0,'DP19_02'=1,'DP20_02'=2,'DP21_02'=3,'DP22_02'=4,'DP23_02'=5,'DP24_02'=6,
                                        'DP26_02'=0,'DP27_02'=1,'DP28_02'=2,'DP29_02'=3,'DP30_02'=4,'DP31_02'=5,'DP32_02'=6))

p2 <-ggplot(data=C2, aes(x=Decisions, y=mean, fill=Decisions))+
  facet_grid(.~Rounds)+
  geom_bar(position=position_dodge(),colour="black", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+geom_line()+
  guides(fill=FALSE)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=16),
                   axis.title.x=element_text(face="bold", size=14),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   strip.text.x = element_text(size = 12))+
  labs(x = "Person A Gives",
       y = "Compensation Intensity")+
  scale_x_discrete(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6,7,8,9,10),limits=c(0,7))+
  scale_fill_brewer(palette='PuBu',direction=-1)
print(p2)

library(cowplot)
punishcomp <- plot_grid(p1, p2, labels = c("A", "B"),nrow=2)
print(punishcomp)


#PUNISHMENT & COMPENSATION#
library(reshape2)
smpc <- melt(data1,
            id.vars='ID',
            measure.vars=c('DP02_01','DP03_01','DP04_01','DP05_01','DP06_01','DP07_01','DP08_01',
                           'DP10_01','DP11_01','DP12_01','DP13_01','DP14_01','DP15_01','DP16_01',
                           'DP18_01','DP19_01','DP20_01','DP21_01','DP22_01','DP23_01','DP24_01',
                           'DP26_01','DP27_01','DP28_01','DP29_01','DP30_01','DP31_01','DP32_01',
                           'DP02_02','DP03_02','DP04_02','DP05_02','DP06_02','DP07_02','DP08_02',
                           'DP10_02','DP11_02','DP12_02','DP13_02','DP14_02','DP15_02','DP16_02',
                           'DP18_02','DP19_02','DP20_02','DP21_02','DP22_02','DP23_02','DP24_02',
                           'DP26_02','DP27_02','DP28_02','DP29_02','DP30_02','DP31_02','DP32_02'),
            variable.name= "Decisions",
            value.name="DVs" 
)

smpc = cbind(smpc,Rounds=rep(smpc$Decisions))
smpc = cbind(smpc,Punish.comp=rep(smpc$Decisions))

library(plyr)
smpc$Rounds <- revalue(smpc$Rounds, c('DP02_01'='Round 1','DP03_01'='Round 1','DP04_01'='Round 1','DP05_01'='Round 1','DP06_01'='Round 1','DP07_01'='Round 1','DP08_01'='Round 1',
                                      'DP10_01'='Round 2','DP11_01'='Round 2','DP12_01'='Round 2','DP13_01'='Round 2','DP14_01'='Round 2','DP15_01'='Round 2','DP16_01'='Round 2',
                                      'DP18_01'='Round 3','DP19_01'='Round 3','DP20_01'='Round 3','DP21_01'='Round 3','DP22_01'='Round 3','DP23_01'='Round 3','DP24_01'='Round 3',
                                      'DP26_01'='Round 4','DP27_01'='Round 4','DP28_01'='Round 4','DP29_01'='Round 4','DP30_01'='Round 4','DP31_01'='Round 4','DP32_01'='Round 4',
                                      'DP02_02'='Round 1','DP03_02'='Round 1','DP04_02'='Round 1','DP05_02'='Round 1','DP06_02'='Round 1','DP07_02'='Round 1','DP08_02'='Round 1',
                                      'DP10_02'='Round 2','DP11_02'='Round 2','DP12_02'='Round 2','DP13_02'='Round 2','DP14_02'='Round 2','DP15_02'='Round 2','DP16_02'='Round 2',                                           
                                      'DP18_02'='Round 3','DP19_02'='Round 3','DP20_02'='Round 3','DP21_02'='Round 3','DP22_02'='Round 3','DP23_02'='Round 3','DP24_02'='Round 3',
                                      'DP26_02'='Round 4','DP27_02'='Round 4','DP28_02'='Round 4','DP29_02'='Round 4','DP30_02'='Round 4','DP31_02'='Round 4','DP32_02'='Round 4'
                                      ))

smpc$Punish.comp <- revalue(smpc$Punish.comp, c('DP02_01'='Punishment','DP03_01'='Punishment','DP04_01'='Punishment','DP05_01'='Punishment','DP06_01'='Punishment','DP07_01'='Punishment','DP08_01'='Punishment',
                                                'DP10_01'='Punishment','DP11_01'='Punishment','DP12_01'='Punishment','DP13_01'='Punishment','DP14_01'='Punishment','DP15_01'='Punishment','DP16_01'='Punishment',
                                                'DP18_01'='Punishment','DP19_01'='Punishment','DP20_01'='Punishment','DP21_01'='Punishment','DP22_01'='Punishment','DP23_01'='Punishment','DP24_01'='Punishment',
                                                'DP26_01'='Punishment','DP27_01'='Punishment','DP28_01'='Punishment','DP29_01'='Punishment','DP30_01'='Punishment','DP31_01'='Punishment','DP32_01'='Punishment',
                                                'DP02_02'='Compensation','DP03_02'='Compensation','DP04_02'='Compensation','DP05_02'='Compensation','DP06_02'='Compensation','DP07_02'='Compensation','DP08_02'='Compensation',
                                                'DP10_02'='Compensation','DP11_02'='Compensation','DP12_02'='Compensation','DP13_02'='Compensation','DP14_02'='Compensation','DP15_02'='Compensation','DP16_02'='Compensation',
                                                'DP18_02'='Compensation','DP19_02'='Compensation','DP20_02'='Compensation','DP21_02'='Compensation','DP22_02'='Compensation','DP23_02'='Compensation','DP24_02'='Compensation',
                                                'DP26_02'='Compensation','DP27_02'='Compensation','DP28_02'='Compensation','DP29_02'='Compensation','DP30_02'='Compensation','DP31_02'='Compensation','DP32_02'='Compensation'))

smpc$Decisions <- revalue(smpc$Decisions, c('DP02_02'=0,'DP03_02'=1,'DP04_02'=2,'DP05_02'=3,'DP06_02'=4,'DP07_02'=5,'DP08_02'=6,
                                            'DP10_02'=0,'DP11_02'=1,'DP12_02'=2,'DP13_02'=3,'DP14_02'=4,'DP15_02'=5,'DP16_02'=6,
                                            'DP18_02'=0,'DP19_02'=1,'DP20_02'=2,'DP21_02'=3,'DP22_02'=4,'DP23_02'=5,'DP24_02'=6,
                                            'DP26_02'=0,'DP27_02'=1,'DP28_02'=2,'DP29_02'=3,'DP30_02'=4,'DP31_02'=5,'DP32_02'=6,
                                            'DP02_01'=0,'DP03_01'=1,'DP04_01'=2,'DP05_01'=3,'DP06_01'=4,'DP07_01'=5,'DP08_01'=6,
                                            'DP10_01'=0,'DP11_01'=1,'DP12_01'=2,'DP13_01'=3,'DP14_01'=4,'DP15_01'=5,'DP16_01'=6,
                                            'DP18_01'=0,'DP19_01'=1,'DP20_01'=2,'DP21_01'=3,'DP22_01'=4,'DP23_01'=5,'DP24_01'=6,
                                            'DP26_01'=0,'DP27_01'=1,'DP28_01'=2,'DP29_01'=3,'DP30_01'=4,'DP31_01'=5,'DP32_01'=6))


library(plyr)
C3<-ddply(smpc, .(Decisions,Rounds,Punish.comp), summarise,
          N    = length(DVs),
          mean = mean(DVs, na.rm=TRUE),
          sd   = sd(DVs,na.rm = TRUE),
          se   = sd / sqrt(N))


library(ggplot2)
library(RColorBrewer)
p3 <-ggplot(data=C3, aes(x=Decisions, y=mean, fill=Punish.comp))+
  facet_grid(.~Rounds)+
  geom_bar(position=position_dodge(),colour="black", stat="identity")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,position = position_dodge(.92))+geom_line()+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   legend.text=element_text(size=12),
                   legend.title=element_text(face="bold",size=14),
                   axis.title.y=element_text(face="bold", size=16),
                   axis.title.x=element_text(face="bold", size=14),
                   axis.text.y=element_text(size=12),
                   axis.text.x=element_text(size=12),
                   strip.text.x = element_text(size = 12))+
  labs(x = "Person A Gives",
       y = "Intervention Intensity")+
  scale_x_discrete(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6,7,8,9,10),limits=c(0,6))+
  scale_fill_brewer(name = "Intervention Type",palette='Set1')
print(p3)


######## EFFECTS #######
library(sjPlot)
library(ggplot2)
library(cowplot)

#H1# PUNISHMENT.
p1.EP1<-plot_model(EP1,type="pred",terms="Ambiguity",title=" ",ci.lvl=0.95,
                   axis.title="Punishment Intensity")+
  geom_line(color='red',linetype = 2,size=1)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12),limits=c(0,12))
p1.EP1

p2.EP1<-plot_model(EP1,type="pred",terms="Uncertainty",title=" ",ci.lvl=0.95,
                   axis.title=" ")+
  geom_line(color='red',linetype = 2,size=1)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12),limits=c(0,12))
p2.EP1

PLOT1 <- plot_grid(p1.EP1,p2.EP1)
PLOT1

#H1# COMPENSATION.
p1.EC1<-plot_model(EC1,type="pred",terms="Ambiguity",title=" ",ci.lvl=0.95,
                   axis.title="Compensation Intensity")+
  geom_line(color='blue',linetype = 2,size=1)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12,14,16),limits=c(0,16))
p1.EC1

p2.EC1<-plot_model(EC1,type="pred",terms="Uncertainty",title=" ",ci.lvl=0.95,
                   axis.title=" ")+
  geom_line(color='blue',linetype = 2,size=1)+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12,14,16),limits=c(0,16))
p2.EC1

PLOT2 <- plot_grid(p1.EC1,p2.EC1)
PLOT2


#H2#
p1.EP2a <- plot_model(EP2a, type = "pred", terms = c("ObserverJS.c", "Ambiguity"),title=" ",
                      axis.title=c("Observer JS","Punishment Intensity"),pretty=FALSE, colors = c("#404040","#CA0020"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   legend.title=element_text(size=14, face = "bold"), 
                   legend.text=element_text(size=14),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12,14,16,18),limits=c(0,18))
p1.EP2a

p1.EC2a <- plot_model(EC2a, type = "pred", terms = c("ObserverJS.c", "Ambiguity"),title=" ",
                      axis.title=c("Observer JS","Punishment Intensity"),pretty=FALSE, colors = c("#404040","#2c7fb8"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   legend.title=element_text(size=14, face = "bold"), 
                   legend.text=element_text(size=14),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,2,4,6,8,10,12,14,16,18),limits=c(0,18))

p1.EC2a

#H3#
p1.EP3 <- plot_model(EP3, type = "eff", terms = c("Punishment.x", "Ambiguity"),title=" ",
                      axis.title=c("Punishment","Intervention Intensity (Retrosp.)"),pretty=FALSE, colors = c("#404040","#CA0020"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   legend.title=element_text(size=14, face = "bold"), 
                   legend.text=element_text(size=14),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),limits=c(-2,2))
p1.EP3

p2.EP3 <- plot_model(EP3, type = "eff", terms = c("Punishment.x", "Uncertainty"),title=" ",
                     axis.title=c("Punishment"," "),pretty=FALSE, colors = c("#404040","#CA0020"))+
  theme_bw()+theme(panel.grid.minor=element_blank(),
                   legend.title=element_text(size=14, face = "bold"), 
                   legend.text=element_text(size=14),
                   axis.title.y=element_text(face="bold", size=18),
                   axis.title.x=element_text(face="bold", size=18),
                   axis.text.y=element_text(size=16),
                   axis.text.x=element_text(size=16),
                   strip.text.x = element_text(size = 12))+
  scale_y_continuous(expand = c(0, 0),limits=c(-2,2))
p2.EP3
PLOT3 <- plot_grid(p1.EP3,p2.EP3)
PLOT3

