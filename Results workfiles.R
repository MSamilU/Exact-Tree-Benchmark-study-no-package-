#Load results
setwd("C:/Users/msami/Dropbox/MS/Scriptie/Results")
load("ACA_Benchmark.Rdata")
load("CCC_Benchmark.Rdata")
load("Cohen_Benchmark.Rdata")
load("MAD_Benchmark.Rdata")
load("RCj_Benchmark.Rdata")

library(ez)
library(ggplot2)
library(effsize)
library(ggpubr)

#Long format conversion 


#RCj
RCj.Benchmark$Breastcancer[,4] <-  paste(names(RCj.Benchmark[1]))
RCj.Benchmark$Ionosphere[,4] <- paste(names(RCj.Benchmark[2]))
RCj.Benchmark$Boston.Housing[,4] <-  paste(names(RCj.Benchmark[3]))
RCj.Benchmark$Abalone[,4] <-  paste(names(RCj.Benchmark[4]))

RCj.long.abalone        <- reshape(RCj.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
RCj.long.Boston.Housing <- reshape(RCj.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
RCj.long.Ionosphere     <- reshape(RCj.Benchmark$Ionosphere, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))
RCj.long.Breastcancer   <- reshape(RCj.Benchmark$Breastcancer, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))



#CCC

CCC.Benchmark$Boston.Housing[,4] <-  paste(names(CCC.Benchmark[3]))
CCC.Benchmark$Abalone[,4] <-  paste(names(CCC.Benchmark[4]))

CCC.long.abalone        <- reshape(CCC.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "CCC", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
CCC.long.Boston.Housing <- reshape(CCC.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "CCC", timevar= "Method", times =  c("rpart", "evtree", "ETree"))


#MAD
MAD.Benchmark$Boston.Housing[,4] <-  paste(names(MAD.Benchmark[3]))
MAD.Benchmark$Abalone[,4] <-  paste(names(MAD.Benchmark[4]))

MAD.long.abalone        <- reshape(MAD.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "MAD", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
MAD.long.Boston.Housing <- reshape(MAD.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "MAD", timevar= "Method", times =  c("rpart", "evtree", "ETree"))


#ACA
ACA.Benchmark$Breastcancer[,4] <-  paste(names(ACA.Benchmark[1]))
ACA.Benchmark$Ionosphere[,4] <- paste(names(ACA.Benchmark[2]))

ACA.long.Ionosphere     <- reshape(ACA.Benchmark$Ionosphere, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "ACA" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))
ACA.long.Breastcancer   <- reshape(ACA.Benchmark$Breastcancer, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "ACA" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))


#Cohen
Cohen.Benchmark$Breastcancer[,4] <-  paste(names(Cohen.Benchmark[1]))
Cohen.Benchmark$Ionosphere[,4] <- paste(names(Cohen.Benchmark[2]))

Cohen.long.Ionosphere     <- reshape(Cohen.Benchmark$Ionosphere, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "Cohen" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))
Cohen.long.Breastcancer   <- reshape(Cohen.Benchmark$Breastcancer, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "Cohen" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))




#####Boxplots####
#RCj
ggarrange(
ggplot(RCj.long.Breastcancer, aes(x=RCj, y=Method , fill = Method)) + geom_boxplot(show.legend = FALSE) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold")) + ggtitle("Breastcancer") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,1),
ggplot(RCj.long.Ionosphere, aes(x=RCj, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ggtitle("Ionosphere")+ theme(plot.title = element_text(hjust = 0.5)) + xlim(0,1),
ggplot(RCj.long.Boston.Housing, aes(x=RCj, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Boston Housing") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,1),
ggplot(RCj.long.abalone, aes(x=RCj, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold")) + ggtitle("Abalone") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,1)

)

#CCC
ggarrange(
ggplot(CCC.long.Boston.Housing, aes(x=CCC, y=Method, fill = Method)) + geom_boxplot( show.legend = FALSE) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Boston Housing") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0.5,1),
ggplot(CCC.long.abalone, aes(x=CCC, y=Method, fill = Method)) + geom_boxplot( show.legend = FALSE) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Abalone") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0.5,1)
)

#MAD

ggarrange(ggplot(MAD.long.Boston.Housing, aes(x=MAD, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Boston Housing") + theme(plot.title = element_text(hjust = 0.5)),
ggplot(MAD.long.abalone, aes(x=MAD, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Abalone") + theme(plot.title = element_text(hjust = 0.5))
)

#ACA
ggarrange(

ggplot(ACA.long.Breastcancer, aes(x=ACA, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Breastcancer") + theme(plot.title = element_text(hjust = 0.5))+ xlim(0.7,1),
ggplot(ACA.long.Ionosphere, aes(x=ACA, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20),axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Ionosphere") + theme(plot.title = element_text(hjust = 0.5))+ xlim(0.7,1)
)

#Cohen
ggarrange(
ggplot(Cohen.long.Breastcancer, aes(x=Cohen, y=Method, fill = Method)) + geom_boxplot(show.legend = FALSE ) +coord_flip() + theme(axis.text = element_text(size = 20) ,axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Breastcancer") + theme(plot.title = element_text(hjust = 0.5))+ xlab("Cohen's Kappa")+ xlim(0,1),
ggplot(Cohen.long.Ionosphere, aes(x=Cohen, y=Method, fill = Method)) + geom_boxplot( show.legend = FALSE) +coord_flip() + theme(axis.text = element_text(size = 20) ,axis.title=element_text(size=14,face="bold"), title=element_text(size=14,face="bold"))+ ggtitle("Ionosphere") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Cohen's Kappa")+ xlim(0,1)
)











######ANOVA ######
#RCj


#binding all long format dataframes into one
RCj.Benchmarklong <- rbind(reshape(RCj.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
,reshape(RCj.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
,reshape(RCj.Benchmark$Ionosphere, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj" , timevar= "Method", times =  c("rpart", "evtree", "ETree"))
,reshape(RCj.Benchmark$Breastcancer, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "RCj" , timevar= "Method", times =  c("rpart", "evtree", "ETree")))

colnames(RCj.Benchmarklong)[1] <- "Dataset"
#means per method across datasets
mean(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "rpart")])
sd(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "rpart")])

mean(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "evtree")])
sd(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "evtree")])

mean(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "ETree")])
sd(RCj.Benchmarklong$RCj[which(RCj.Benchmarklong$Method == "ETree")])



anova.RCj<- ezANOVA(data= RCj.Benchmarklong,dv= RCj , within = Method, between = Dataset , wid = id, detailed = TRUE)
anova.RCj

#eta squared calculations
anova.RCj$ANOVA$SSn[2]/ (anova.RCj$ANOVA$SSn[2] +anova.RCj$ANOVA$SSn[3] + anova.RCj$ANOVA$SSn[4]) 
anova.RCj$ANOVA$SSn[3]/ (anova.RCj$ANOVA$SSn[2] +anova.RCj$ANOVA$SSn[3] + anova.RCj$ANOVA$SSn[4]) 
anova.RCj$ANOVA$SSn[4]/ (anova.RCj$ANOVA$SSn[2] +anova.RCj$ANOVA$SSn[3] + anova.RCj$ANOVA$SSn[4]) 

#Post hoc comparisons

pairwise.t.test(RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Abalone", "RCj"], RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Abalone","Method"], p.adj = "holm", paired=TRUE)

pairwise.t.test(RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Boston.Housing","RCj"], RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Boston.Housing","Method"], p.adj = "holm", paired=TRUE)

pairwise.t.test(RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Breastcancer","RCj"], RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Breastcancer","Method"], p.adj = "holm", paired=TRUE)

pairwise.t.test(RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Ionosphere","RCj"], RCj.Benchmarklong[RCj.Benchmarklong$Dataset=="Ionosphere", "Method"], p.adj = "holm", paired=TRUE)

mean(RCj.Benchmark$Abalone$rpart)
mean(RCj.Benchmark$Abalone$evtree)
mean(RCj.Benchmark$Abalone$ETree)

mean(RCj.Benchmark$Breastcancer$rpart)
mean(RCj.Benchmark$Breastcancer$evtree)
mean(RCj.Benchmark$Breastcancer$ETree)

mean(RCj.Benchmark$Ionosphere$rpart)
mean(RCj.Benchmark$Ionosphere$evtree)
mean(RCj.Benchmark$Ionosphere$ETree)

t.test(RCj.Benchmark$Breastcancer$evtree, RCj.Benchmark$Breastcancer$rpart, paired = TRUE, alternative = "two.sided")



#ANOVA CCC
#binding all long format dataframes into one
CCC.Benchmarklong <- rbind(reshape(CCC.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "CCC", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
                           ,reshape(CCC.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "CCC", timevar= "Method", times =  c("rpart", "evtree", "ETree")))

colnames(CCC.Benchmarklong)[1] <- "Dataset"


#means per method across datasets
mean(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "rpart")])
sd(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "rpart")])


mean(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "evtree")])
sd(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "evtree")])

mean(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "ETree")])
sd(CCC.Benchmarklong$CCC[which(CCC.Benchmarklong$Method == "ETree")])



anova.CCC<- ezANOVA(data= CCC.Benchmarklong,dv= CCC , within = Method , between = Dataset , wid = id, detailed = TRUE)
anova.CCC


#eta squared calculations
anova.CCC$ANOVA$SSn[2]/ (anova.CCC$ANOVA$SSn[2] +anova.CCC$ANOVA$SSn[3] + anova.CCC$ANOVA$SSn[4]) 
anova.CCC$ANOVA$SSn[3]/ (anova.CCC$ANOVA$SSn[2] +anova.CCC$ANOVA$SSn[3] + anova.CCC$ANOVA$SSn[4]) 
anova.CCC$ANOVA$SSn[4]/ (anova.CCC$ANOVA$SSn[2] +anova.CCC$ANOVA$SSn[3] + anova.CCC$ANOVA$SSn[4]) 


#post hoc t tests
pairwise.t.test(CCC.Benchmarklong[, "CCC"], CCC.Benchmarklong[,"Method"], p.adj = "holm", paired=TRUE)

##Wilcoxon ACA
#Function for Effect size R
rFromWilcox<-function(wilcoxModel,N)
{
  z<-qnorm(wilcoxModel$p.value/2)
  r<-z/sqrt(N)
  cat(wilcoxModel$data.name,"Z-value =", z ,", Effect Size, r=",r)
}

#Breastcancer

median(ACA.Benchmark$Breastcancer$rpart)
median(ACA.Benchmark$Breastcancer$evtree)
median(ACA.Benchmark$Breastcancer$ETree)

ACA.wilcox.breastcancer.rpart <- wilcox.test(ACA.Benchmark$Breastcancer$ETree, ACA.Benchmark$Breastcancer$rpart, paired = TRUE, alternative = "two.sided")

ACA.wilcox.breastcancer.evtree <- wilcox.test(ACA.Benchmark$Breastcancer$ETree, ACA.Benchmark$Breastcancer$evtree, paired = TRUE, alternative = "two.sided")

ACA.wilcox.breastcancer.rpart
ACA.wilcox.breastcancer.evtree

rFromWilcox(ACA.wilcox.breastcancer.rpart, 400)

rFromWilcox(ACA.wilcox.breastcancer.evtree, 400)



#Ionosphere
median(ACA.Benchmark$Ionosphere$rpart)
median(ACA.Benchmark$Ionosphere$evtree)
median(ACA.Benchmark$Ionosphere$ETree)

ACA.wilcox.ionosphere.rpart <- wilcox.test(ACA.Benchmark$Ionosphere$ETree, ACA.Benchmark$Ionosphere$rpart, paired = TRUE, alternative = "two.sided")

ACA.wilcox.ionosphere.evtree <- wilcox.test(ACA.Benchmark$Ionosphere$ETree, ACA.Benchmark$Ionosphere$evtree, paired = TRUE, alternative = "two.sided")

ACA.wilcox.ionosphere.rpart
ACA.wilcox.ionosphere.evtree

rFromWilcox(ACA.wilcox.ionosphere.rpart, 400)
rFromWilcox(ACA.wilcox.ionosphere.evtree, 400)






##Wilcoxon Cohen's Kappa

#Breastcancer

median(Cohen.Benchmark$Breastcancer$rpart)
median(Cohen.Benchmark$Breastcancer$evtree)
median(Cohen.Benchmark$Breastcancer$ETree)


cohen.wilcox.breastcancer.rpart <- wilcox.test(Cohen.Benchmark$Breastcancer$ETree, Cohen.Benchmark$Breastcancer$rpart, paired = TRUE, alternative = "two.sided")

cohen.wilcox.breastcancer.evtree <- wilcox.test(Cohen.Benchmark$Breastcancer$ETree, Cohen.Benchmark$Breastcancer$evtree, paired = TRUE, alternative = "two.sided")



rFromWilcox(cohen.wilcox.breastcancer.rpart, 400)

rFromWilcox(cohen.wilcox.breastcancer.evtree, 400)



#Ionosphere

median(Cohen.Benchmark$Ionosphere$rpart)
median(Cohen.Benchmark$Ionosphere$evtree)
median(Cohen.Benchmark$Ionosphere$ETree)



cohen.wilcox.ionosphere.rpart <- wilcox.test(Cohen.Benchmark$Ionosphere$ETree, Cohen.Benchmark$Ionosphere$rpart, paired = TRUE, alternative = "two.sided")

cohen.wilcox.ionosphere.evtree <- wilcox.test(Cohen.Benchmark$Ionosphere$ETree, Cohen.Benchmark$Ionosphere$evtree, paired = TRUE, alternative = "two.sided")

rFromWilcox(cohen.wilcox.ionosphere.rpart, 400)
rFromWilcox(cohen.wilcox.ionosphere.evtree, 400)



#Paired t-tests MAD and effect size cohen's d#
MAD.Benchmarklong <- rbind(reshape(MAD.Benchmark$Abalone, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "MAD", timevar= "Method", times =  c("rpart", "evtree", "ETree"))
                           ,reshape(MAD.Benchmark$Boston.Housing, direction = "long", varying = c("rpart", "evtree", "ETree"), v.names = "MAD", timevar= "Method", times =  c("rpart", "evtree", "ETree")))

colnames(MAD.Benchmarklong)[1] <- "Dataset"
#Boston Housing
t.test(MAD.Benchmark$Boston.Housing$ETree, MAD.Benchmark$Boston.Housing$rpart, paired = TRUE, alternative = "two.sided")
t.test(MAD.Benchmark$Boston.Housing$ETree, MAD.Benchmark$Boston.Housing$evtree, paired = TRUE, alternative = "two.sided")

pairwise.t.test(MAD.Benchmarklong[MAD.Benchmarklong$Dataset=="Boston.Housing","MAD"], MAD.Benchmarklong[MAD.Benchmarklong$Dataset=="Boston.Housing","Method"], p.adj = "holm", paired=TRUE)



cohen.d(MAD.Benchmark$Boston.Housing$ETree, MAD.Benchmark$Boston.Housing$rpart, paired = TRUE)
cohen.d(MAD.Benchmark$Boston.Housing$ETree, MAD.Benchmark$Boston.Housing$evtree, paired = TRUE)




#Abalone
t.test(MAD.Benchmark$Abalone$ETree, MAD.Benchmark$Abalone$rpart, paired = TRUE, alternative = "two.sided")
t.test(MAD.Benchmark$Abalone$ETree, MAD.Benchmark$Abalone$evtree, paired = TRUE, alternative = "two.sided")
pairwise.t.test(MAD.Benchmarklong[MAD.Benchmarklong$Dataset=="Abalone", "MAD"], MAD.Benchmarklong[MAD.Benchmarklong$Dataset=="Abalone","Method"], p.adj = "holm", paired=TRUE)


cohen.d(MAD.Benchmark$Abalone$ETree, MAD.Benchmark$Abalone$rpart, paired = TRUE)
cohen.d(MAD.Benchmark$Abalone$ETree, MAD.Benchmark$Abalone$evtree, paired = TRUE)








