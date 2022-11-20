install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
install.packages("scales")                                   # Install and load scales
library("scales")
install.packages("ineq")
library(ineq)
install.packages("glmdisc")
library(glmdisc)
install.packages("ggrepel")
library(ggrepel)

data1 <- read_excel("/Users/pshayan/Downloads/Pn.xlsx",  sheet = "Data")
data2 <- read_excel("/Users/pshayan/Downloads/Bb.xlsx", sheet = "Data")
data3 <- read_excel("/Users/pshayan/Downloads/Cc.xlsx", sheet = "Data")

############## Sd-Mean-Gini :  Question ##############

df1<- data.frame(data1)
df1
apply(df1,2,sd)
mp<-apply(df1,2,mean)
mp
gp<-apply(df1,2,Gini)
gp
summary(df1)
#apply(df1,2,gini.coef)
#x<- as.numeric(unlist(df1$Q26))
#ineq(x,type="Gini")
#x<- runif(df1)
#Gini(x)




df2<- data.frame(data2)
apply(df2,2,sd)
mb<-apply(df2,2,mean)
mb
gb<-apply(df2,2,Gini)
gb
#apply(df2,2,gini.coef)
summary(df2)

df3<- data.frame(data3)
apply(df3,2,sd)
mc<-apply(df3,2,mean)
mc
gc<-apply(df3,2,Gini)
gc
#apply(df3,2,gini.coef)
summary(df3)


######## Plots- Questions ############################

points <- rbind(cbind(mp, gp), cbind(mb, gb), cbind(mc, gc))
colnames(points) <- c("mean","gini")
rownames(points) <- c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10",
                      "P11","P12","P13","P14","P15","P16","P17","P18","P19","P20",
                      "P21","P22","P23","P24","P25","P26","P27","P28","P29","P30",
                      "B1","B2","B3","B4","B5","B6","B7","B8","B9","B10",
                      "B11","B12","B13","B14","B15","B16","B17","B18","B19","B20",
                      "B21","B22","B23","B24","B25","B26","B27","B28","B29","B30",
                      "C1","C2","C3","C4","C5","C6","C7","C8","C9","C10",
                      "C11","C12","C13","C14","C15","C16","C17","C18","C19","C20",
                      "C21","C22","C23","C24","C25","C26","C27","C28","C29","C30")
View(points)

points <- as.data.frame(points)
dataset <- c(rep("PayamNoor", 30),rep("BlackBoard", 30),rep("Canvas", 30))
construct <- c(rep("PU",6),rep("PEU",5),rep("BIT",4),rep("ATUT",8),rep("ATU",7),
               rep("PU",6),rep("PEU",5),rep("BIT",4),rep("ATUT",8),rep("ATU",7),
               rep("PU",6),rep("PEU",5),rep("BIT",4),rep("ATUT",8),rep("ATU",7))

points <- cbind(points,dataset,construct)
View(points)

ggplot(points, aes(x=mean, y=gini, shape=construct, color=dataset)) +
  theme_bw() +
  geom_point(size=5) +
 geom_text_repel(label=rownames(points), size=4, 
                  nudge_x = 0.002, colour="black") +
 # geom_text_repel(label=rownames(points), size=4, vjust="left", hjust="left", 
 #                nudge_x = 0.002, colour="black") +
  xlab("Mean") +
  ylab("Gini Heterogeneity Index") +
  ggtitle("Questions Descriptives summary") +
  theme(plot.title = element_text(face="bold", size=26, hjust=0.5),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text=element_text(size=16),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20))+
  labs(shape="Construct", colour="LMS dataset")


############# Sd-Mean-Gini : Construct ###############

################### PN ######################
PU<-rowMeans(data1[,c(1:6)])
sd(PU)
mp1<-mean(PU)
mp1
gp1<-Gini(PU)
gp1
#gini.coef(PU)
summary(PU)
PEU<-rowMeans(data1[,c(7:11)])
sd(PEU)
mp2<-mean(PEU)
gp2<-Gini(PEU)
#gini.coef(PEU)
summary(PEU)
BIT<-rowMeans(data1[,c(12:15)])
sd(BIT)
mp3<-mean(BIT)
gp3<-Gini(BIT)
#gini.coef(BIT)
summary(BIT)
ATUT<-rowMeans(data1[,c(16:23)])
sd(ATUT)
mp4<-mean(ATUT)
#gini.coef(ATUT)
gp4<-Gini(ATUT)
summary(ATUT)
ATU<-rowMeans(data1[,c(24:30)])
sd(ATU)
mp5<-mean(ATU)
gp5<-Gini(ATU)
#gini.coef(ATU)
summary(ATU)

################## BB ######################
PU<-rowMeans(data2[,c(1:6)])
sd(PU)
mb1<-mean(PU)
mb1
gb1<-Gini(PU)
gb1
#gini.coef(PU)
summary(PU)
PEU<-rowMeans(data2[,c(7:11)])
sd(PEU)
mb2<-mean(PEU)
gb2<-Gini(PEU)
#gini.coef(PEU)
summary(PEU)
BIT<-rowMeans(data2[,c(12:15)])
sd(BIT)
mb3<-mean(BIT)
gb3<-Gini(BIT)
#gini.coef(BIT)
summary(BIT)
ATUT<-rowMeans(data2[,c(16:23)])
sd(ATUT)
mb4<-mean(ATUT)
gb4<-Gini(ATUT)
#gini.coef(ATUT)
summary(ATUT)
ATU<-rowMeans(data2[,c(24:30)])
sd(ATU)
mb5<-mean(ATU)
gb5<-Gini(ATU)
#gini.coef(ATU)
summary(ATU)
################# Cc #####################
PU<-rowMeans(data3[,c(1:6)])
sd(PU)
mc1<-mean(PU)
mc1
gc1<-Gini(PU)
gc1
#gini.coef(PU)
summary(PU)
PEU<-rowMeans(data3[,c(7:11)])
sd(PEU)
mc2<-mean(PEU)
gc2<-Gini(PEU)
#gini.coef(PEU)
summary(PEU)
BIT<-rowMeans(data3[,c(12:15)])
sd(BIT)
mc3<-mean(BIT)
gc3<-Gini(BIT)
#gini.coef(BIT)
summary(BIT)
ATUT<-rowMeans(data3[,c(16:23)])
sd(ATUT)
mc4<-mean(ATUT)
gc4<-Gini(ATUT)
#gini.coef(ATUT)
summary(ATUT)
ATU<-rowMeans(data3[,c(24:30)])
sd(ATU)
mc5<-mean(ATU)
gc5<-Gini(ATU)
#gini.coef(ATU)
summary(ATU)

################ plots-Constructs ####################

mp0<-c(mp1,mp2,mp3,mp4,mp5)
gp0<-c(gp1,gp2,gp3,gp4,gp5)

mb0<-c(mb1,mb2,mb3,mb4,mb5)
gb0<-c(gb1,gb2,gb3,gb4,gb5)

mc0<-c(mc1,mc2,mc3,mc4,mc5)
gc0<-c(gc1,gc2,gc3,gc4,gc5)

mpbc0<-c(mp0,mb0,mc0)
mpbc<-as.numeric(mpbc0)
gpbc0<-c(gp0,gb0,gc0)
gpbc<-as.numeric(gpbc0)

#points <- rbind(cbind(mp0, gp0), cbind(mb0, gb0), cbind(mc0, gc0))
#colnames(points) <- c("mean","gini")

points <- cbind(mpbc, gpbc)
View(points)

points <- as.data.frame(points)

dataset <- c(rep("PayamNoor",5),rep("Blackboard",5),rep("Canvas",5))
construct <- c("PU","PEU","BIT","ATUT","ATU",
                "PU","PEU","BIT","ATUT","ATU",
                "PU","PEU","BIT","ATUT","ATU")

points <- cbind(points,dataset,construct)
View(points)

ggplot(points, aes(x=mpbc, y=gpbc, shape=construct, color=dataset)) +
  theme_bw() +
  geom_point(size=8) +
# geom_text_repel(label=rownames(points), size=4, 
#              nudge_x = 0.002, colour="black") +
# geom_text_repel(label=rownames(points), size=4, vjust="left", hjust="left", 
#                nudge_x = 0.002, colour="black") +
  xlab("Mean") +
  ylab("Gini Heterogeneity Index") +
  ggtitle("Constructs Descriptives summary") +
  theme(plot.title = element_text(face="bold", size=26, hjust=0.5),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text=element_text(size=16),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20))+
  labs(shape="Construct", colour="LMS dataset")



######## ANOVA ################################

new_data1=matrix(0, nrow = 5, ncol = ncol(data1))
for (i in 1:ncol(data1)) {
  
  #the number of observation in each category
  number.observation.category<-as.data.frame(table(data1[,i]))
  new_data1[,i]<-number.observation.category[,2]
}


new_data2<-data.frame(cbind("Sample"=c("1","2","3","4" ,"5"),"Status"=c("0","0","0","0","0")))
for (j in 1:ncol(data2)) {
  
  number.observation.category<-as.data.frame(table(data2[,j]))
  number.observation.category
  b1<-data.frame("Sample"=number.observation.category$Var1,"Status"=number.observation.category$Freq)
  c1 <- merge(new_data2, b1, by = "Sample", all=TRUE)
  new_data2<-c1
}

# replace NA value with zero
new_data2<- as.matrix(new_data2[,-c(1,2)])
new_data2[is.na(new_data2)] <- 0



new_data3<-data.frame(cbind("Sample"=c("1","2","3","4" ,"5"),"Status"=c("0","0","0","0","0")))
for (j in 1:ncol(data3)) {
  
  number.observation.category<-as.data.frame(table(data3[,j]))
  number.observation.category
  b1<-data.frame("Sample"=number.observation.category$Var1,"Status"=number.observation.category$Freq)
  c1 <- merge(new_data3, b1, by = "Sample", all=TRUE)
  new_data3<-c1
}

# replace NA value with zero
new_data3<- as.matrix(new_data3[,-c(1,2)])
new_data3[is.na(new_data3)] <- 0

######################### ANOVA Questions #############################

d1<-data.frame(cbind(new_data1[,1], new_data2[,1], new_data3[,1]))
stk1<-stack(d1)
Q1<-aov(values~ind,data = stk1)
summary(Q1)
TukeyHSD(Q1, conf.level=.95)
plot(TukeyHSD(Q1, conf.level=.95), las = 2)

d2<-data.frame(cbind(new_data1[,2], new_data2[,2], new_data3[,2]))
stk2<-stack(d2)
Q2<-aov(values~ind,data = stk2)
summary(Q2)
TukeyHSD(Q2, conf.level=.95)
plot(TukeyHSD(Q2, conf.level=.95), las = 2)

d3<-data.frame(cbind(new_data1[,3], new_data2[,3], new_data3[,3]))
stk3<-stack(d3)
Q3<-aov(values~ind,data = stk3)
summary(Q3)
TukeyHSD(Q3, conf.level=.95)
plot(TukeyHSD(Q3, conf.level=.95), las = 2)

d4<-data.frame(cbind(new_data1[,4], new_data2[,4], new_data3[,4]))
stk4<-stack(d4)
Q4<-aov(values~ind,data = stk4)
summary(Q4)
TukeyHSD(Q4, conf.level=.95)
plot(TukeyHSD(Q4, conf.level=.95), las = 2)

d5<-data.frame(cbind(new_data1[,5], new_data2[,5], new_data3[,5]))
stk5<-stack(d5)
Q5<-aov(values~ind,data = stk5)
summary(Q5)
TukeyHSD(Q5, conf.level=.95)
plot(TukeyHSD(Q5, conf.level=.95), las = 2)

d6<-data.frame(cbind(new_data1[,6], new_data2[,6], new_data3[,6]))
stk6<-stack(d6)
Q6<-aov(values~ind,data = stk6)
summary(Q6)
TukeyHSD(Q6, conf.level=.95)
plot(TukeyHSD(Q6, conf.level=.95), las = 2)

d7<-data.frame(cbind(new_data1[,7], new_data2[,7], new_data3[,7]))
stk7<-stack(d7)
Q7<-aov(values~ind,data = stk7)
summary(Q7)
TukeyHSD(Q7, conf.level=.95)
plot(TukeyHSD(Q7, conf.level=.95), las = 2)

d8<-data.frame(cbind(new_data1[,8], new_data2[,8], new_data3[,8]))
stk8<-stack(d8)
Q8<-aov(values~ind,data = stk8)
summary(Q8)
TukeyHSD(Q8, conf.level=.95)
plot(TukeyHSD(Q8, conf.level=.95), las = 2)

d9<-data.frame(cbind(new_data1[,9], new_data2[,9], new_data3[,9]))
stk9<-stack(d9)
Q9<-aov(values~ind,data = stk9)
summary(Q9)
TukeyHSD(Q9, conf.level=.95)
plot(TukeyHSD(Q9, conf.level=.95), las = 2)

d10<-data.frame(cbind(new_data1[,10], new_data2[,10], new_data3[,10]))
stk10<-stack(d10)
Q10<-aov(values~ind,data = stk10)
summary(Q10)
TukeyHSD(Q10, conf.level=.95)
plot(TukeyHSD(Q10, conf.level=.95), las = 2)

d11<-data.frame(cbind(new_data1[,11], new_data2[,11], new_data3[,11]))
stk11<-stack(d11)
Q11<-aov(values~ind,data = stk11)
summary(Q11)
TukeyHSD(Q11, conf.level=.95)
plot(TukeyHSD(Q11, conf.level=.95), las = 2)

d12<-data.frame(cbind(new_data1[,12], new_data2[,12], new_data3[,12]))
stk12<-stack(d12)
Q12<-aov(values~ind,data = stk12)
summary(Q12)
TukeyHSD(Q12, conf.level=.95)
plot(TukeyHSD(Q12, conf.level=.95), las = 2)

d13<-data.frame(cbind(new_data1[,13], new_data2[,13], new_data3[,13]))
stk13<-stack(d13)
Q13<-aov(values~ind,data = stk13)
summary(Q13)
TukeyHSD(Q13, conf.level=.95)
plot(TukeyHSD(Q13, conf.level=.95), las = 2)

d14<-data.frame(cbind(new_data1[,14], new_data2[,14], new_data3[,14]))
stk14<-stack(d14)
Q14<-aov(values~ind,data = stk14)
summary(Q14)
TukeyHSD(Q14, conf.level=.95)
plot(TukeyHSD(Q14, conf.level=.95), las = 2)

d15<-data.frame(cbind(new_data1[,15], new_data2[,15], new_data3[,15]))
stk15<-stack(d15)
Q15<-aov(values~ind,data = stk15)
summary(Q15)
TukeyHSD(Q15, conf.level=.95)
plot(TukeyHSD(Q15, conf.level=.95), las = 2)

d16<-data.frame(cbind(new_data1[,16], new_data2[,16], new_data3[,16]))
stk16<-stack(d16)
Q16<-aov(values~ind,data = stk16)
summary(Q16)
TukeyHSD(Q16, conf.level=.95)
plot(TukeyHSD(Q16, conf.level=.95), las = 2)

d17<-data.frame(cbind(new_data1[,17], new_data2[,17], new_data3[,17]))
stk17<-stack(d17)
Q17<-aov(values~ind,data = stk17)
summary(Q17)
TukeyHSD(Q17, conf.level=.95)
plot(TukeyHSD(Q17, conf.level=.95), las = 2)

d18<-data.frame(cbind(new_data1[,18], new_data2[,18], new_data3[,18]))
stk18<-stack(d18)
Q18<-aov(values~ind,data = stk18)
summary(Q18)
TukeyHSD(Q18, conf.level=.95)
plot(TukeyHSD(Q18, conf.level=.95), las = 2)

d19<-data.frame(cbind(new_data1[,19], new_data2[,19], new_data3[,19]))
stk19<-stack(d19)
Q19<-aov(values~ind,data = stk19)
summary(Q19)
TukeyHSD(Q19, conf.level=.95)
plot(TukeyHSD(Q19, conf.level=.95), las = 2)

d20<-data.frame(cbind(new_data1[,20], new_data2[,20], new_data3[,20]))
stk20<-stack(d20)
Q20<-aov(values~ind,data = stk20)
summary(Q20)
TukeyHSD(Q20, conf.level=.95)
plot(TukeyHSD(Q20, conf.level=.95), las = 2)

d21<-data.frame(cbind(new_data1[,21], new_data2[,21], new_data3[,21]))
stk21<-stack(d21)
Q21<-aov(values~ind,data = stk21)
summary(Q21)
TukeyHSD(Q21, conf.level=.95)
plot(TukeyHSD(Q21, conf.level=.95), las = 2)

d22<-data.frame(cbind(new_data1[,22], new_data2[,22], new_data3[,22]))
stk22<-stack(d22)
Q22<-aov(values~ind,data = stk22)
summary(Q22)
TukeyHSD(Q22, conf.level=.95)
plot(TukeyHSD(Q22, conf.level=.95), las = 2)

d23<-data.frame(cbind(new_data1[,23], new_data2[,23], new_data3[,23]))
stk23<-stack(d23)
Q23<-aov(values~ind,data = stk23)
summary(Q23)
TukeyHSD(Q23, conf.level=.95)
plot(TukeyHSD(Q23, conf.level=.95), las = 2)

d24<-data.frame(cbind(new_data1[,24], new_data2[,24], new_data3[,24]))
stk24<-stack(d24)
Q24<-aov(values~ind,data = stk24)
summary(Q24)
TukeyHSD(Q24, conf.level=.95)
plot(TukeyHSD(Q24, conf.level=.95), las = 2)

d25<-data.frame(cbind(new_data1[,25], new_data2[,25], new_data3[,25]))
stk25<-stack(d25)
Q25<-aov(values~ind,data = stk25)
summary(Q25)
TukeyHSD(Q25, conf.level=.95)
plot(TukeyHSD(Q25, conf.level=.95), las = 2)

d26<-data.frame(cbind(new_data1[,26], new_data2[,26], new_data3[,26]))
stk26<-stack(d26)
Q26<-aov(values~ind,data = stk26)
summary(Q26)
TukeyHSD(Q26, conf.level=.95)
plot(TukeyHSD(Q26, conf.level=.95), las = 2)

d27<-data.frame(cbind(new_data1[,27], new_data2[,27], new_data3[,27]))
stk27<-stack(d27)
Q27<-aov(values~ind,data = stk27)
summary(Q27)
TukeyHSD(Q27, conf.level=.95)
plot(TukeyHSD(Q27, conf.level=.95), las = 2)

d28<-data.frame(cbind(new_data1[,28], new_data2[,28], new_data3[,28]))
stk28<-stack(d28)
Q28<-aov(values~ind,data = stk28)
summary(Q28)
TukeyHSD(Q28, conf.level=.95)
plot(TukeyHSD(Q28, conf.level=.95), las = 2)

d29<-data.frame(cbind(new_data1[,29], new_data2[,29], new_data3[,29]))
stk29<-stack(d29)
Q29<-aov(values~ind,data = stk29)
summary(Q29)
TukeyHSD(Q29, conf.level=.95)
plot(TukeyHSD(Q29, conf.level=.95), las = 2)

d30<-data.frame(cbind(new_data1[,30], new_data2[,30], new_data3[,30]))
stk30<-stack(d30)
Q30<-aov(values~ind,data = stk30)
summary(Q30)
TukeyHSD(Q30, conf.level=.95)
plot(TukeyHSD(Q30, conf.level=.95), las = 2)
par(mfcol = c(2, 2))

########################  ANOVA Constructs ################################

d16<-data.frame(cbind(new_data1[,1:6], new_data2[,1:6], new_data3[,1:6]))
stk16<-stack(d16)
PU<-aov(values~ind,data = stk16)
summary(PU)
TukeyHSD(PU, conf.level=.95)
plot(TukeyHSD(PU, conf.level=.95), las = 2)

d711<-data.frame(cbind(new_data1[,7:11], new_data2[,7:11], new_data3[,7:11]))
stk711<-stack(d711)
PEU<-aov(values~ind,data = stk711)
summary(PEU)
TukeyHSD(PEU, conf.level=.95)
plot(TukeyHSD(PEU, conf.level=.95), las = 2)

d1215<-data.frame(cbind(new_data1[,12:15], new_data2[,12:15], new_data3[,12:15]))
stk1215<-stack(d1215)
BIT<-aov(values~ind,data = stk1215)
summary(BIT)
TukeyHSD(BIT, conf.level=.95)
plot(TukeyHSD(BIT, conf.level=.95), las = 2)

d1623<-data.frame(cbind(new_data1[,16:23], new_data2[,16:23], new_data3[,16:23]))
stk1623<-stack(d1623)
ATUT<-aov(values~ind,data = stk1623)
summary(ATUT)
TukeyHSD(ATUT, conf.level=.95)
plot(TukeyHSD(ATUT, conf.level=.95), las = 2)

d2430<-data.frame(cbind(new_data1[,24:30], new_data2[,24:30], new_data3[,24:30]))
stk2430<-stack(d2430)
ATU<-aov(values~ind,data = stk2430)
summary(ATU)
TukeyHSD(ATU, conf.level=.95)
plot(TukeyHSD(ATU, conf.level=.95), las = 2)



################# Extra ######################

colors = c("brown4","brown3","lightcyan2","lightcyan3","lightcyan4")
colors = c("slategray1","slategray2","steelblue2","steelblue3","steelblue4")
q <- c("Q1","Q2","Q3","Q4","Q5","Q6","PU")
s <- c("strongly disagree","disagree","neutral", "agree","strongly agree")
x <- matrix(c(3.9,0.0,0.0,2.0,3.9,9.8,3.3,
                   13.7,2.0,2.0,17.6,11.8,13.7,10.1,
                   17.6,0.0,0.0,43.1,27.5,17.6,17.6,
                   51.0,39.2,47.1,27.5,43.1,49.0,42.8,
                   13.7,58.8,51.0,9.8,13.7,9.8,26.1), nrow = 5, ncol = 7, byrow = TRUE)

barplot(x, main = "Perceived Usefulness (PU)", names.arg = q, col = colors)
legend("bottom",s, inset=-0.3, cex = 0.53, fill = colors, horiz = TRUE, xpd=TRUE, text.width = 1.5)
