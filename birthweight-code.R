#Dinh My Ky
#STAT452

library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(ggmap)
setwd("D:/2.Year2/Semester3/STAT452/Dataforfinal")
df<-read.csv("bwght2.csv",header=TRUE)
attach(df)
str(df)

isIntChar<-function(x)
{
  return(all(is.character(x)))
}

df <- df %>% mutate_if(isIntChar, as.integer)
str(df)

#print(df)
#df<-as.data.frame(apply(df, 2, as.numeric))
df[is.na(df)]<-as.integer(0)
str(df)
attach(df)

table(fwhte)

##Qualitative variables:
# male
#change level 
male<-as.factor(male)
levels(male)<-c("Female", "Male")
male
#frequency table
maletable=table(male)
maletable
#pie

# try type.data
type.data <- data.frame(c=1:1832)
type.male = 1:length(male);

for (i in 1:length(male))
{
  if (male[i] == 0)
    type.male[i] = "Female"
  else
    type.male[i] = "Male"
}

type.data$type.male<-type.male
type.data
class(type.data)

typemaledf<-type.data%>%
  group_by(type.male)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typemaledf

malepie<-ggplot(typemaledf, aes(x="",y=ratioVal,fill=type.male))+
  theme_bw()+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0)+
  ggtitle("Baby's gender ratio")+
  scale_fill_brewer(name = "Gender", labels = c("Female","Male"), palette="Oranges")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
malepie

#lbw
type.lbw = 1:length(lbw);
for (i in 1:length(lbw)){
  if (lbw[i] == 0){
    type.lbw[i] = "Not low";
  }else{
    type.lbw[i] = "Low";
  }
}
type.data$type.lbw<-type.lbw
typelbwdf<-type.data%>%
  group_by(type.lbw)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typelbwdf
lbwpie<-ggplot(typelbwdf, aes(x="",y=ratioVal,fill=type.lbw))+
  theme_bw()+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0)+
  ggtitle("Low birthweight ratio")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "Low birthweight", labels = c("Low","Not low"), values=c("#f48833","#6fea95"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
lbwpie

#vlbw
type.vlbw = 1:length(vlbw);

for (i in 1:length(vlbw)){
  if (vlbw[i] == 0){
    type.vlbw[i] = "Not very low";
  }else{
    type.vlbw[i] = "Very low";
  }
}

type.data$type.vlbw<-type.vlbw
type.data
class(type.data)

typevlbwdf<-type.data%>%
  group_by(type.vlbw)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))

typevlbwdf
vlbwpie<-ggplot(typevlbwdf, aes(x="",y=ratioVal,fill=type.vlbw))+
  theme_bw()+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0)+
  ggtitle("Very low birthweight ratio")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "Very low birthweight", labels = c("Not very low","Very low"), values=c("#f98aa4", "#f1eb92"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
vlbwpie

#mblck
type.mblck = 1:length(mblck);
for (i in 1:length(mblck)){
  if (mblck[i] == 0){
    type.mblck[i] = "Not black";
  }else{
    type.mblck[i] = "Black";
  }
}
type.data$type.mblck<-type.mblck
type.data
typemblckdf<-type.data%>%
  group_by(type.mblck)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typemblckdf
mblckpie<-ggplot(typemblckdf, aes(x="",y=ratioVal,fill=type.mblck))+
  theme_bw()+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0)+
  ggtitle("Black mother ratio")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "Black mother", labels = c("Black","Not black"), values=c("#7fa5fb","#f8f2a6"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
mblckpie

#mwhte
#White 89%, not white 11%
type.mwhte = 1:length(mwhte);
for (i in 1:length(mwhte)){
  if (mwhte[i] == 0)
    type.mwhte[i] = "Not white"
  else
    type.mwhte[i] = "White"
}
type.data$type.mwhte<-type.mwhte
typemwhtedf<-type.data%>%
  group_by(type.mwhte)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typemwhtedf

mwhtepie<-ggplot(typemwhtedf, aes(x="",y=ratioVal,fill=type.mwhte))+
  theme_light()+
  geom_bar(width = 2, stat = "identity") +
  coord_polar("y", start=0)+
  ggtitle("White mother ratio")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "White mother", labels = c("Not white","White"), values=c("#63c1f5","#b1ddf5"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
mwhtepie

#moth
#not other 95%, other 5%
type.moth = 1:length(moth);

for (i in 1:length(moth)){
  if (moth[i] == 0){
    type.moth[i] = "Not other";
  }else{
    type.moth[i] = "Other";
  }
}

type.data$type.moth<-type.moth
type.data
class(type.data)

typemothdf<-type.data%>%
  group_by(type.moth)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))

typemothdf

mothpie<-ggplot(typemothdf, aes(x="",y=ratioVal,fill=type.moth))+
  theme_grey()+
  geom_bar(width = 2, stat = "identity") +
  coord_polar("y", start=0)+
  ggtitle("Other mother ratio")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "Other mother", labels = c("Not other","Other"), values=c("#f8f2a6","#efe23d"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))

mothpie

#Quali-Quali
#omaps 
type.omaps = 1:length(omaps);
for (i in 1:length(omaps)){
  if (omaps[i]>=0 && omaps[i]<=3)
    type.omaps[i] = "1. Lifesaving"
  else if (omaps[i]>=4 && omaps[i]<=6)
    type.omaps[i] = "2. Breathing assistance"
  else type.omaps[i] = "3. Routine care"
}
type.data$type.omaps<-type.omaps
typeomapsdf<-type.data%>%
  group_by(type.omaps)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typeomapsdf

table(type.omaps)

omapspie<-ggplot(typeomapsdf, aes(x="",y=ratioVal,fill=type.omaps))+
  theme_bw()+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0)+
  ggtitle("1-minute APGAR score")+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  scale_fill_manual(name = "omaps", labels = c("Breathing assistance","Lifesaving measures","Routine care"), values=c("#1c85bf","#63c1f5","#b1ddf5"))+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))

omapspie

#omaps_lbw
#Group by omaps and lbw
omaps_lbw<-type.data %>%
  group_by(type.omaps, type.lbw) %>%
  summarise(count=n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal)) #%>%
omaps_lbw
#Plot
omaps_lbw_bar<-ggplot(omaps_lbw, aes(x=type.omaps, y=perc, fill=type.lbw))+
  theme_light()+
  geom_col(width=0.7,    
           position=position_dodge(0.7))+
  xlab("1-minute APGAR")+
  ylab("Percentage")+
  ggtitle("Low birth weights by 1-minute APGAR range")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#D55E00","#63c1f5"))
omaps_lbw_bar

#fmaps
type.fmaps = 1:length(fmaps);
for (i in 1:length(fmaps)){
  if (omaps[i]>=0 && fmaps[i]<=3)
    type.fmaps[i] = "1. Low"
  else if (fmaps[i]>=4 && fmaps[i]<=6)
    type.fmaps[i] = "2. Moderately abnormal"
  else type.fmaps[i] = "3. Normal"
}
type.data$type.fmaps<-type.fmaps
type.data
typefmapsdf<-type.data%>%
  group_by(type.fmaps)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal)) 
typefmapsdf

#fmaps_lbw
fmaps_lbw<-type.data %>%
  group_by(type.fmaps, type.lbw) %>%
  summarise(count=n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
fmaps_lbw
fmaps_lbw_bar<-ggplot(fmaps_lbw, aes(x=type.fmaps, y=perc, fill=type.lbw))+
  theme_light()+
  geom_col(width=0.7,    
           position=position_dodge(0.7))+
  xlab("5-minute APGAR")+
  ylab("Percentage")+
  ggtitle("Low birth weights by 5-minute APGAR range")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#f83a67","#f8ef84"))
fmaps_lbw_bar

#male_lbw
male_lbw<-type.data %>%
  group_by(type.male, type.lbw) %>%
  summarise(count=n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
male_lbw
male_lbw_bar<-ggplot(male_lbw, aes(x=type.male, y=count, fill=type.lbw))+
  #theme_grey()+
  geom_col(width=0.7,    
           position=position_dodge(0.7))+
  xlab("Baby's gender")+
  ylab("Frequency")+
  ggtitle("Low birth weights by baby's gender")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#D55E00","#63c1f5"))+
  scale_y_continuous(breaks = seq(0, 900, by = 100))
male_lbw_bar

#mblck_lbw

mblck_lbw<-type.data %>%
  group_by(type.mblck, type.lbw) %>%
  summarise(count=n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
mblck_lbw
mblck_lbw_bar<-ggplot(mblck_lbw, aes(x=type.mblck, y=count, fill=type.lbw))+
  theme_grey()+
  geom_col(width=0.7,    
           position=position_dodge(0.7))+
  xlab("Low birth weights by black mother")+
  ylab("Frequency")+
  ggtitle("Low birth weights by baby's gender")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#6fea95","#f48833"))+
  #break into smaller units to easily observe the small value
  scale_y_continuous(breaks = seq(0, 1750, by = 100))
mblck_lbw_bar

mblck_lbw_pie<-ggplot(mblck_lbw, aes(x="", y=ratioVal, fill=type.lbw))+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0) + facet_wrap(~type.mblck,ncol = 2,scale =
                                             "fixed")+
  ggtitle("Low birth weights by baby's gender")+
  xlab("")+
  ylab("")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#6fea95","#f48833"))+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
mblck_lbw_pie

typemblckdf



##Quantitative
#bwght



summary(df$mage)
hist(mage, breaks = 30)

summary(df$fage)
hist(fage, breaks = 30)

summary(df$meduc)
hist(meduc, breaks = 30)

#find mode
#print mode and fequency
library ("DescTools")
mod_val <- Mode(df$meduc)
print(mod_val)

length(which(meduc==12))

summary(df$feduc)
hist(feduc, breaks = 30)

summary(df$monpre)
hist(monpre, breaks = 30)

summary(df$npvis)
hist(npvis, breaks = 30)

#bwght
summary(df$bwght)
hist(bwght, breaks = 30)

library ("DescTools")
mod_val <- Mode(df$bwght)
print(mod_val)

library(moments)
skewness(bwght)
skewness(lbwght)

bwghtHist<-ggplot(df,aes(x=bwght)) +
geom_histogram(aes(y=..density..),fill="#69b3a2", color="#e9ecef", alpha=0.9) +
geom_density(alpha=0.9) +
xlab("Birth weight")+
ylab("Density")+
ggtitle("Histogram of birth weight")
bwghtHist


bwghtBox<-ggplot(df,aes(x=bwght)) +
  geom_boxplot(fill = "#FFDB6D", color = "#C4961A") +
  #geom_density(alpha=0.9) +
  xlab("Birth weight")+
  ylab("Density")+
  ggtitle("Boxplot of birth weight")
bwghtBox

outlierBwght<-boxplot.stats(bwght)$out
outlierBwght
length(outlierBwght)

ggplot(df,aes(x=bwght)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  #geom_density(alpha=0.5) +
  scale_x_continuous("Birth weight")+
  ylab("Frequency") +
  ggtitle("Histogram of birth weight")#+
  
  #plot(density(bwght),add=TRUE)
#stat_function(fun = dnorm, args = list(mean = mean(df$bwght), sd = sd(df$bwght)))
#+
#scale_fill_discrete(name = "Benh tim", labels = c("Duong tinh","Am tinh"))


summary(df$omaps)
hist(omaps, breaks = 30)

summary(df$fmaps)
hist(fmaps, breaks = 30)

#cigs
summary(df$cigs)
table(cigs)
hist(cigs, breaks = 30)

modcigs <- Mode(df$cigs)
print(modcigs)

cigsHist<-ggplot(df,aes(x=cigs)) +
  geom_histogram(bins=40, aes(y=..density..),fill="#51bdf8", color="#b4d9d1", alpha=0.9) +
  geom_density(alpha=0.9) +
  xlab("Average cigarettes per day")+
  ylab("Density")+
  ggtitle("Histogram of parent's cigarettes per day")
cigsHist

cigsHist<-ggplot(df,aes(x=cigs)) +
  geom_histogram(bins=40, fill="#51bdf8", color="#b4d9d1", alpha=0.9) +
  #geom_density(alpha=0.9) +
  xlab("Average cigarettes per day")+
  ylab("Density")+
  ggtitle("Histogram of parent's average cigarettes per day")
cigsHist

cigsBox<-ggplot(df,aes(x=cigs)) +
  geom_boxplot(fill = c("red")) +
  xlab("Log birth weight")+
  ylab("Density")+
  ggtitle("Boxplot of parent's average cigarettes per day")
cigsBox

outlierCigs<-boxplot.stats(cigs)$out
outlierCigs
length(outlierCigs)
summary(cigs)

#drinks
summary(df$drink)
hist(drink, breaks = 30)

table(drink)
outlierdrink<-boxplot.stats(drink)$out
outlierdrink
length(outlierdrink)

boxplot(drink)

drinkHist<-ggplot(df,aes(x=drink)) +
  geom_histogram(bins=40, aes(y=..density..),fill="#51bdf8", color="#b4d9d1") +
  geom_density(alpha=0.9) +
  xlab("Average drinks per week")+
  ylab("Density")+
  ggtitle("Histogram of parent's drinks per week")
drinkHist

drinkHist<-ggplot(df,aes(x=drink)) +
  geom_histogram(bins=40, fill="#f86423", color="#b9f823", alpha=0.8) +
  #geom_density(alpha=0.9) +
  xlab("Average drinks per week")+
  ylab("Density")+
  ggtitle("Histogram of mother's drinks per week")
drinkHist

drinkHist<-ggplot(df,aes(x=drink)) +
  geom_histogram(bins=40, aes(y=..density..),fill="#f86423", color="#ff5e39", alpha=0.8) +
  geom_density(size=0.46)+
  xlab("Average drinks per week")+
  ylab("Density")+
  ggtitle("Histogram of mother's drinks per week")
drinkHist

drinkBox<-ggplot(df,aes(x=drink)) +
  geom_boxplot(fill = c("red")) +
  xlab("Average drinks per week")+
  ylab("Density")+
  ggtitle("Boxplot of mother's drinks per week ")
drinkBox

#lbwght
summary(df$lbwght)
hist(lbwght, breaks = 30)

library ("DescTools")
modlbwght <- Mode(df$lbwght)
print(modlbwght)

lbwghtHist<-ggplot(df,aes(x=lbwght)) +
  geom_histogram(bins=40, aes(y=..density..),fill="#51bdf8", color="#b4d9d1", alpha=0.9) +
  geom_density(alpha=0.9) +
  xlab("Log of birth weight")+
  ylab("Density")+
  ggtitle("Histogram of log birth weight")
lbwghtHist

lbwghtHist<-ggplot(df,aes(x=lbwght)) +
  geom_histogram(bins=40, fill="#51bdf8", color="#b4d9d1", alpha=0.9) +
  #geom_density(alpha=0.9) +
  xlab("Log of birth weight")+
  ylab("Density")+
  ggtitle("Histogram of log birth weight")
lbwghtHist

lbwghtBox<-ggplot(df,aes(x=lbwght)) +
  geom_boxplot(fill = c("#f774ca")) +
  xlab("Log birth weight")+
  ylab("Density")+
  ggtitle("Boxplot of log birth weight")
lbwghtBox

outlierLbwght<-boxplot.stats(lbwght)$out
length(outlierLbwght)


boxplot(lbwght)

outlierLbwght<-boxplot.stats(lbwght)$out
length(outlierLbwght)

boxplot(mage)
summary(mage)
summary(meduc)

length(which(mage>35))

#mage test 
summary(mage)
hist(mage, breaks=30)

t.test(mage, mu=24, alternative="greater")

mageHist<-ggplot(df,aes(x=mage)) +
  geom_histogram(fill="#ffe980", color="#fcd18d", alpha=0.9) +
  xlab("Mother's age")+
  ylab("Frequency")+
  ggtitle("Histogram of mother's age")
mageHist
skewness(mage)

#npvis test
mean(npvis)
hist(npvis, breaks=30)
boxplot(npvis)

npvisHist<-ggplot(df,aes(x=npvis)) +
  geom_histogram(fill="#6cdc49", color="#b4d9d1", alpha=0.9) +
  xlab("Total number of prenatal visits")+
  ylab("Frequency")+
  ggtitle("Histogram of number of prenatal visits")
npvisHist

npvisBox<-ggplot(df,aes(x=npvis)) +
  geom_boxplot(fill = c("#6cdc49")) +
  xlab("Log birth weight")+
  ylab("Density")+
  ggtitle("Boxplot of log birth weight")
npvisBox

summary(npvis)

mod_val <- Mode(df$npvis)
print(mod_val)

t.test(npvis, mu=20, alternative="less")

#test quali
#mwhte
typemwhtedf
prop.test(x=1624, n=1832, p=0.6, alternative = "greater")

#vlbw
typevlbwdf
vlbwpie
prop.test(x=13, n=1832, p=0.08, alternative="less")

#monpre
monpreHist<-ggplot(df,aes(x=monpre)) +
  geom_histogram(bins=40, aes(y=..density..),fill="#51bdf8", color="#b4d9d1", alpha=0.9) +
  geom_density(alpha=0.9) +
  xlab("Log of birth weight")+
  ylab("Density")+
  ggtitle("Histogram of log birth weight")
monpreHist

summary(monpre)
hist(monpre, breaks=30)
table(monpre)


#Simple regression model

library(ggplot2)
library(tidyverse)

set.seed(1)
sample<-sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train<-df[sample, ]
test<-df[!sample, ]

train
model1<-lm(train$bwght~train$mage)
summary(model1)
model1
confint(model1)

#p1<-data.frame(prediction1)
#RMSE(p1$fit, test$bwght)
#R2_Score(p1$fit,test$bwght)

test$bwght

prediction1<-predict(model1,data.frame(test$mage), interval = "confidence")
prediction1


pred<-data.frame(model1 %>% predict(data.frame(test$mage), interval="confidence"))
pred
class(pred)

library(Metrics)
rmse(test$bwght,pred$fit)
library(lattice)
library(caret)
#R2(pred$fit, test$bwght)
pred$fit

avr_y_actual <- mean(test$bwght)

# 2.2. Total sum of squares
ss_total <- sum((test$bwght - avr_y_actual)^2)

# 2.3. Regression sum of squares
ss_regression <- sum((pred$fit - avr_y_actual)^2)

# 2.4. Residual sum of squares
ss_residuals <- sum((test$bwght - pred$fit)^2)

# 3. R2 Score
r2 <- 1 - ss_residuals / ss_total

r2

pred$fit
test$bwght

library(MLmetrics)
R2_Score(pred$fit, test$bwght)
RMSE(pred$fit, test$bwght)

training.samples <- df$bwght %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

modelextra<-lm(train.data$bwght ~ train.data$mage)
modelextra
predextra<-predict(modelextra, data.frame(test.data$mage),interval = "confidence")
predextra
library(lattice)
library(caret)
RMSE(predextra,test.data$bwght)
R2_Score(predextra, test.data$bwght)


plot(mage, bwght)
cor(mage, bwght) #0.03368266

cor(train$mage, train$bwght)

model1

plotMage<-ggplot(train,aes(train$mage,train$bwght,color = train$mage))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Mother's age") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and mother's age")
plotMage

summary(meduc)

plot(meduc,bwght)
cor(meduc,bwght) #0.02831425
plotMeduc<-ggplot(train,aes(train$meduc,train$bwght,color = train$meduc))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Mother's education") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and mother's education")
plotMeduc

model2<-lm(train$bwght~train$meduc)
summary(model2)
model2
confint(model2)

prediction2<-predict(model2,data.frame(test$meduc), interval = "confidence")
prediction2

p2<-data.frame(prediction2)
RMSE(p2$fit, test$bwght)
R2_Score(p2$fit,test$bwght)

#monpre
plot(monpre,bwght)
cor(monpre,bwght)#0.0004151357

plotMonpre<-ggplot(train,aes(train$monpre,train$bwght,color = train$monpre))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Mother's prenatal care") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and month prenatal care")
plotMonpre

model3<-lm(train$bwght~train$monpre)
summary(model3)
model3
confint(model3)

prediction3<-predict(model3,data.frame(test$monpre), interval = "confidence")
prediction3

p3<-data.frame(prediction3)
RMSE(p3$fit, test$bwght)
R2_Score(p3$fit,test$bwght)

#npvis
plot(npvis,bwght)
cor(npvis,bwght)#0.08550077
plotNpvis<-ggplot(train,aes(train$npvis,train$bwght,color = train$npvis))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Number of prenatal visits") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and number of prenatal visits")#+
  #stat_smooth()
plotNpvis

model4<-lm(train$bwght~train$npvis)
summary(model4)
model4
confint(model4)

prediction4<-predict(model4,data.frame(test$npvis), interval = "confidence")
prediction4

p4<-data.frame(prediction4)
RMSE(p4$fit, test$bwght)
R2_Score(p4$fit,test$bwght)

plot(fage,bwght)
cor(fage,bwght) #0.05818073

#fage
plotFage<-ggplot(train,aes(train$fage,train$bwght,color = train$fage))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Father's age") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and father's age")#+
  #stat_smooth()
plotFage

model5<-lm(train$bwght~train$fage)
summary(model5)
model5
confint(model5)

prediction5<-predict(model5,data.frame(test$fage), interval = "confidence")
prediction5

p5<-data.frame(prediction5)
RMSE(p5$fit, test$bwght)
R2_Score(p5$fit,test$bwght)

#feduc
plotFeduc<-ggplot(train,aes(train$feduc,train$bwght,color = train$feduc))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Father's education") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and father's education")
plotFeduc

model6<-lm(train$bwght~train$feduc)
summary(model6)
model6
confint(model6)

prediction6<-predict(model6,data.frame(test$feduc), interval = "confidence")
prediction6

p6<-data.frame(prediction6)
RMSE(p6$fit, test$bwght)
R2_Score(p6$fit,test$bwght)

#ompas
plot(omaps,bwght)
cor(omaps,bwght) #0.2177571
plotOmaps<-ggplot(train,aes(train$omaps,train$bwght,color = train$omaps))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("1-minute APGAR score") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and 1-minute APGAR score")
plotOmaps

model7<-lm(train$bwght~train$omaps)
summary(model7)
model7
confint(model7)

prediction7<-predict(model7,data.frame(test$omaps), interval = "confidence")
prediction7

p7<-data.frame(prediction7)
RMSE(p7$fit, test$bwght)
R2_Score(p7$fit,test$bwght)

#fmaps
plot(fmaps,bwght)
cor(fmaps,bwght) #0.2420019
plot(omaps,bwght)
cor(omaps,bwght) #0.2177571
plotFmaps<-ggplot(train,aes(train$fmaps,train$bwght,color = train$fmaps))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("5-minute APGAR score") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and 5-minute APGAR score")
plotFmaps

model8<-lm(train$bwght~train$fmaps)
summary(model8)
model8
confint(model8)

prediction8<-predict(model8,data.frame(test$fmaps), interval = "confidence")
prediction8

p8<-data.frame(prediction8)
RMSE(p8$fit, test$bwght)
R2_Score(p8$fit,test$bwght)

modelfmaps<-lm(bwght~fmaps)
modelfmaps

#cigs
plot(cigs,bwght)
cor(cigs,bwght) #-0.07783557
plotCigs<-ggplot(train,aes(train$cigs,train$bwght,color = train$cigs))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Average cigarettes per day") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and parent's cigarettes per day")
plotCigs

model9<-lm(train$bwght~train$cigs)
summary(model9)
model9
confint(model9)

prediction9<-predict(model9,data.frame(test$cigs), interval = "confidence")
prediction9

p9<-data.frame(prediction9)
RMSE(p9$fit, test$bwght)
R2_Score(p9$fit,test$bwght)

#drink
plot(drink,bwght)
cor(drink,bwght) #-0.01819645
plotDrink<-ggplot(train,aes(train$drink,train$bwght,color = train$drink))+
  geom_point(alpha = 1)+
  geom_smooth(method = "lm", color="#fd4716") +
  xlab("Average drinks per week") +
  ylab("Baby's birth weight")+
  ggtitle("Scatterplot of birth weight and mother's drinks per week")
plotDrink

model10<-lm(train$bwght~train$drink)
summary(model10)
model10
confint(model10)

prediction10<-predict(model10,data.frame(test$drink), interval = "confidence")
prediction10

p10<-data.frame(prediction10)
RMSE(p10$fit, test$bwght)
R2_Score(p10$fit,test$bwght)

#multiple
library(caret)
featurePlot(x = df[ , c("mage", "meduc", "monpre")], y = df$bwght)

model1
modelMul<-lm(bwght~fmaps+omaps+cigs+fage+mage+npvis+meduc+feduc+monpre+drink)
modelMul
summary(modelMul)
confint(modelMul)

#goodness of fit test 
table_fblck_lbw<-table(fblck, lbw)
table_fblck_lbw
chisq.test(table_fblck_lbw)
chisq.test(table_fblck_lbw, simulate.p.value = TRUE)

#fblck_lbw
type.fblck = 1:length(fblck);
for (i in 1:length(fblck)){
  if (fblck[i] == 0){
    type.fblck[i] = "Not black";
  }else{
    type.fblck[i] = "Black";
  }
}
type.data$type.fblck<-type.fblck
type.data
typefblckdf<-type.data%>%
  group_by(type.fblck)%>%
  summarise(count = n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
typefblckdf

fblck_lbw<-type.data %>%
  group_by(type.fblck, type.lbw) %>%
  summarise(count=n()) %>%
  mutate(ratioVal=count/sum(count)) %>%
  mutate(perc=scales::percent(ratioVal))
fblck_lbw
fblck_lbw_bar<-ggplot(fblck_lbw, aes(x=type.fblck, y=ratioVal*100, fill=type.lbw))+
  #theme_grey()+
  geom_col(width=0.7,    
           position=position_dodge(0.7))+
  xlab("Low birth weights by black father")+
  ylab("Frequency")+
  ggtitle("Low birth weights by baby's gender")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#f48833","#6fea95"))
fblck_lbw_bar

fblck_lbw_pie<-ggplot(fblck_lbw, aes(x="", y=ratioVal, fill=type.lbw))+
  geom_bar(width = 2, stat = "identity", color="white") +
  coord_polar("y", start=0) + facet_wrap(~type.fblck,ncol = 2,scale =
                                           "fixed")+
  ggtitle("Low birth weights by black father")+
  xlab("")+
  ylab("")+
  scale_fill_manual(name="Low birth weight", labels=c("Low","Not low"),values=c("#6fea95","#f48833"))+
  theme(plot.title=element_text(hjust=0.5, size=15),
        axis.title=element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())+
  geom_text(aes(label=perc),position = position_stack(vjust = 0.5))
fblck_lbw_pie

#male_vlbw
table_male_vlbw<-table(male, vlbw)
table_male_vlbw
chisq.test(table_male_vlbw)