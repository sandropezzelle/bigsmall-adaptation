# LOAD LIBRARIES

# Plots saved as:
# 5x7 / portrait

# plots
library(ggplot2)

# stat model
library(lme4)
library(lmerTest)
library(MuMIn)
library(survey)
library(sjPlot)
library(plyr)
library(dplyr)


# TO-DOs:
# 1. add t-test comparing DFT in blocks A,B,C,D in each of two orders
# 2. add descriptive statistics table


######################
######################
########EXP2##########
######################
######################

# SET MY WORKING DIRECTORY
mypath <- "../aggregated_data/"
setwd(mypath)

# LOAD DATA FROM .CSV
data40 <- read.csv("Rdata40.csv")
head(data40)

# FORMAT FACTORIAL VARIABLES TO FACTOR
data40$partID <- as.factor(data40$partID)
data40$score <- as.factor(data40$score)
data40$imgID <- as.factor(data40$imgID)
sapply(data40,class)

# TABLE WITH COUNT OF CORRECT/WRONG ASSESSMENTS (see TAB 4)
FtableDF<-data.frame(data40$score,data40$condition)
table(FtableDF) 
FtableDF<-data.frame(table(FtableDF))
FtableDF # COUNT
FtableDF$Freq/640 # PROPORTION

# C: MEAN, SD CORRECT ASSESSMENTS
Fp<-data.frame(data40$score,data40$partID,data40$condition)
Fp2 <- Fp[Fp$data40.score=="1", ]
Ftb<-data.frame(table(Fp2))
Ftb2 <- Ftb[Ftb$Freq!="0", ]
F2 <- Ftb2[Ftb2$data40.condition=="C", ]
# MEAN
mean(F2$Freq)
# SD
sd(F2$Freq)
# MEDIAN
median(F2$Freq)

# Q: MEAN, SD CORRECT ASSESSMENTS
F2Q <- Ftb2[Ftb2$data40.condition=="Q", ]
# MEAN
mean(F2Q$Freq)
# SD
sd(F2Q$Freq)
# MEDIAN
median(F2Q$Freq)


# PREPARE DATA FOR ANALYSIS AND PLOTS
tabALL<-data.frame(data40$score,data40$condition,data40$Ntrial)
FtabALL<-data.frame(table(tabALL))   
myALL <- FtabALL[FtabALL$data40.score=="1", ]
myALL$Ntrial <- as.numeric(as.character(myALL$data40.Ntrial))

# Q NORM ALIGNMENT SCORES: MEAN & STD + MEDIAN
myQ <- myALL[myALL$data40.condition=="Q", ]
exp2Q <- myQ$Freq/20
# ALIGNMENT PER TRIAL
exp2Q
# MEAN
mean(exp2Q)
# SD
sd(exp2Q)
# MEDIAN
median(exp2Q)

# C NORM ALIGNMENT SCORES: MEAN & STD + MEDIAN
myC <- myALL[myALL$data40.condition=="C", ]
exp2C <- myC$Freq/20
# ALIGNMENT PER TRIAL
exp2C
# MEAN
mean(exp2C)
# SD
sd(exp2C)
# MEDIAN
median(exp2C)

# BOXPLOT (FIG 5)
FIG5 <- ggplot(myALL, aes(x=data40.condition, y=Freq/20,
                          fill=data40.condition)) + geom_boxplot() +
  theme(legend.position = "top") + xlab("") +
  ylab("participants-bot alignment") +
  scale_y_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.2,1)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") + 
  annotate("text", x = "Q", y = 0.5, label = "chance", vjust = -0.5, hjust = -1) +
  labs(fill = "condition")
FIG5

FIG5 + geom_jitter(color="blue", size=1.5)

### T-TESTS AND WILCOXON TESTS
# Q: TEST ALIGNMENT > 0.5
# Q: TEST NORMALITY DISTRIBUTION 
shapiro.test(exp2Q)
# p < 0.05 --> non normal!
# Q: ONE-SIDED WILCOXON TEST
wilcox.test(exp2Q, mu = 0.5)
# p < 0.5 --> significantly higher than chance
# Q: ONE-SIDED T-TEST (not in the paper)
# t.test(exp2Q, mu = 0.5)

# C: TEST ALIGNMENT > 0.5
# C: TEST NORMALITY DISTRIBUTION 
shapiro.test(exp2C)
# p > 0.05 -->normal-like!
# C: ONE-SIDED T-TEST
t.test(exp2C, mu = 0.5)
# p < 0.5 --> significantly higher than chance
# C: ONE-SIDED WILCOXON TEST (not in paper)
# wilcox.test(exp2C, mu = 0.5)

# SCATTERPLOT (FIG 6)
FIG6 <- ggplot(myALL, aes(x=Ntrial, y=Freq/20,
                             color=data40.condition, 
                             fill=data40.condition)) + 
  geom_point() +
  geom_smooth(method="lm") +
  xlab("seen trials") +
  ylab("participants-bot alignment") +  
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24,28,32), limits=c(1,32)) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), limits=c(0,1)) +
  labs(fill = "condition") +
  labs(color = "condition") + theme(legend.position = "top") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  annotate("text", x = 29, y = 0.44, label = "chance", vjust = -0.5)
FIG6

# SCATTERPLOT ORDER A (not in paper)
subsetA <- subset(data40, data40$order=="A")
tabA<-data.frame(subsetA$score,subsetA$condition,subsetA$Ntrial)
FtabA<-data.frame(table(tabA))   
myA <- FtabA[FtabA$subsetA.score=="1", ]
myA$Ntrial <- as.numeric(as.character(myA$subsetA.Ntrial))


FIG6A <- ggplot(myA, aes(x=Ntrial, y=Freq/10,
                          color=subsetA.condition, 
                          fill=subsetA.condition)) + 
  geom_point() +
  geom_smooth(method="lm") +
  xlab("seen trials") +
  ylab("participants-bot alignment") +  
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24,28,32), limits=c(1,32)) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), limits=c(0,1)) +
  labs(fill = "condition") +
  labs(color = "condition") + theme(legend.position = "top") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  annotate("text", x = 29, y = 0.44, label = "chance", vjust = -0.5)
FIG6A


# SCATTERPLOT ORDER B (not in paper)
subsetB <- subset(data40, data40$order=="B")
tabB<-data.frame(subsetB$score,subsetB$condition,subsetB$Ntrial)
FtabB<-data.frame(table(tabB))   
myB <- FtabB[FtabB$subsetB.score=="1", ]
myB$Ntrial <- as.numeric(as.character(myB$subsetB.Ntrial))


FIG6B <- ggplot(myB, aes(x=Ntrial, y=Freq/10,
                         color=subsetB.condition,
                         fill=subsetB.condition)) + 
  geom_point() +
  geom_smooth(method="lm") +
  xlab("seen trials") +
  ylab("participants-bot alignment") +  
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24,28,32), limits=c(1,32)) +
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1), limits=c(0,1)) +
  labs(fill = "condition") +
  labs(color = "condition") + theme(legend.position = "top") +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  annotate("text", x = 29, y = 0.44, label = "chance", vjust = -0.5)
FIG6B


# BOXPLOT ORDER A (not in paper)
FIG5A <- ggplot(myA, aes(x=subsetA.condition, y=Freq/10,
                          fill=subsetA.condition)) + geom_boxplot() +
  theme(legend.position = "top") + xlab("") +
  ylab("participants-bot alignment") +
  scale_y_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.2,1)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") + 
  annotate("text", x = "Q", y = 0.5, label = "chance", vjust = -0.5, hjust = -1) +
  labs(fill = "condition")
FIG5A


# BOXPLOT ORDER B (not in paper)
FIG5B <- ggplot(myB, aes(x=subsetB.condition, y=Freq/10,
                         fill=subsetB.condition)) + geom_boxplot() +
  theme(legend.position = "top") + xlab("") +
  ylab("participants-bot alignment") +
  scale_y_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.2,1)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") + 
  annotate("text", x = "Q", y = 0.5, label = "chance", vjust = -0.5, hjust = -1) +
  labs(fill = "condition")
FIG5B


######################
#STATISTICAL MODELING#
######################


######## NULL MODEL
### O FIXED
### 2 RANDOM

null0model <- glmer(score ~ 1
                    + (1|partID)
                    + (1|imgID),
                    data = data40,family = binomial)

summary(null0model)

# PRINT OR, AIC, BIC, etc.
tab_model(null0model)

######## BASE MODEL
### 12 FIXED
### 2 RANDOM


modelCQ0noint <- glmer(score ~ Ntrial # 1. experience with task
                           + DFT # 2. perceptual difficulty
                           + condition # 3. role of condition
                           + Tshape # 4. shape
                           + Tcolor # 5. color
                           + Tsize # 6. GT size
                           + NTOTobjs # 7.# objects image
                           + NRSobjs # 8. # objects RS
                           + NTOTcolors # 9.# total colors
                           + NsamesizeRS # 10. # same-size objects RS
                           + NdiffsizeRS # 11. # diff-size objects RS
                           + order # 12. order of presentation (A/B)
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(modelCQ0noint)



modelCQ1noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       + Tshape # 4. shape
                       + Tcolor # 5. color
                       + Tsize # 6. GT size
                       + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ1noint)
anova(modelCQ0noint,modelCQ1noint,test="Chisq")


modelCQ2noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       # + Tshape # 4. shape
                       + Tcolor # 5. color
                       + Tsize # 6. GT size
                       + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ2noint)
anova(modelCQ1noint,modelCQ2noint,test="Chisq")

modelCQ3noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       # + Tshape # 4. shape
                       # + Tcolor # 5. color
                       + Tsize # 6. GT size
                       + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ3noint)
anova(modelCQ2noint,modelCQ3noint,test="Chisq")



modelCQ4noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       # + Tshape # 4. shape
                       # + Tcolor # 5. color
                       + Tsize # 6. GT size
                       # + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ4noint)
anova(modelCQ3noint,modelCQ4noint,test="Chisq")




modelCQ5noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       # + Tshape # 4. shape
                       # + Tcolor # 5. color
                       + Tsize # 6. GT size
                       # + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       # + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ5noint)
anova(modelCQ4noint,modelCQ5noint,test="Chisq")


modelCQ6noint <- glmer(score ~ Ntrial # 1. experience with task
                       + DFT # 2. perceptual difficulty
                       + condition # 3. role of condition
                       # + Tshape # 4. shape
                       # + Tcolor # 5. color
                       + Tsize # 6. GT size
                       # + NTOTobjs # 7.# objects image
                       + NRSobjs # 8. # objects RS
                       # + NTOTcolors # 9.# total colors
                       + NsamesizeRS # 10. # same-size objects RS
                       # + NdiffsizeRS # 11. # diff-size objects RS
                       # + order # 12. order of presentation (A/B)
                       + (1|partID)
                       + (1|imgID),
                       data = data40, family = binomial,
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=100000)))

# summary(modelCQ6noint)
anova(modelCQ5noint,modelCQ6noint,test="Chisq")



modelCQ7noint <- glmer(score ~ Ntrial
                           + DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           # + NTOTobjs # 6.
                           # + NRSobjs # 7.
                           # + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           # + NdiffsizeRS # 10.
                           # + DFT # 11.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))


anova(modelCQ6noint,modelCQ7noint,test="Chisq")
# THIS IS THE BEST BASE MODEL
summary(modelCQ7noint)

anova(modelCQ0noint,modelCQ7noint,test="Chisq")

# PRINT OR, AIC, BIC, etc.
tab_model(modelCQ7noint)

######## INTERACTION MODEL
### 12 FIXED
### 2 RANDOM
### INTERACTION Ntrial * DFT


int_modelCQ0order <- glmer(score ~ Ntrial*DFT # 1-2. experience with task*DFT
                           + condition # 3. role of condition
                           + Tshape # 4. shape
                           + Tcolor # 5. color
                           + Tsize # 6. GT size
                           + NTOTobjs # 7.
                           + NRSobjs # 8.
                           + NTOTcolors # 9.
                           + NsamesizeRS # 10.
                           + NdiffsizeRS # 11.
                           + order # 12.
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ0order)


int_modelCQ1order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           + Tshape # 3. shape
                           + Tcolor # 4. color
                           + Tsize # 5. GT size
                           + NTOTobjs # 6.
                           + NRSobjs # 7.
                           + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ1order)
anova(int_modelCQ0order,int_modelCQ1order,test="Chisq")



int_modelCQ2order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           + Tcolor # 4. color
                           + Tsize # 5. GT size
                           + NTOTobjs # 6.
                           + NRSobjs # 7.
                           + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ2order)
anova(int_modelCQ2order,int_modelCQ1order,test="Chisq")


int_modelCQ3order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           + NTOTobjs # 6.
                           + NRSobjs # 7.
                           + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ3order)
anova(int_modelCQ2order,int_modelCQ3order,test="Chisq")



int_modelCQ4order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           # + NTOTobjs # 6.
                           + NRSobjs # 7.
                           + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ4order)
anova(int_modelCQ4order,int_modelCQ3order,test="Chisq")



int_modelCQ5order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           # + NTOTobjs # 6.
                           # + NRSobjs # 7.
                           + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ5order)
anova(int_modelCQ4order,int_modelCQ5order,test="Chisq")

int_modelCQ6order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           # + NTOTobjs # 6.
                           # + NRSobjs # 7.
                           # + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

# summary(int_modelCQ6order)
anova(int_modelCQ6order,int_modelCQ5order,test="Chisq")


int_modelCQ7order <- glmer(score ~ Ntrial*DFT # 1. experience with task
                           + condition # 2. role of condition
                           # + Tshape # 3. shape
                           # + Tcolor # 4. color
                           + Tsize # 5. GT size
                           # + NTOTobjs # 6.
                           # + NRSobjs # 7.
                           # + NTOTcolors # 8.
                           + NsamesizeRS # 9.
                           # + NdiffsizeRS # 10.
                           # + order
                           + (1|partID)
                           + (1|imgID),
                           data = data40, family = binomial,
                           control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=100000)))

anova(int_modelCQ6order,int_modelCQ7order,test="Chisq")
# THIS IS MY BEST INTERACTION MODEL
summary(int_modelCQ7order)

anova(int_modelCQ0order,int_modelCQ7order,test="Chisq")

# PRINT OR, AIC, BIC, etc.
tab_model(int_modelCQ7order)



#### COMPARISON TESTS

# COMPARISON BETWEEN NULL AND BASE
anova(null0model,modelCQ7noint,test="Chisq")

# COMPARISON BETWEEN NULL AND INTERACTION
anova(null0model,int_modelCQ7order,test="Chisq")

# COMPARISON BETWEEN BASE AND INTERACTION
anova(modelCQ7noint,int_modelCQ7order,test="Chisq")



######################
##QUESTION ANALYSIS###
######################


data40Q <- subset(data40, data40$condition == "Q")

data40Qyes <- subset(data40Q, data40Q$Qevent == "yes")

sapply(data40Qyes, class)
data40Qyes


mytableINF <- table(data40Qyes$Ntrial, data40Qyes$informativeness)
mytableINF
t2 = as.data.frame(mytableINF)
t2

# Plot showing trends of resolutive/non-resolutive questions through time

t2$Var2 <- revalue(t2$Var2, c("no"="non-resolutive", "yes"="resolutive"))
t2$Var2

p3 <- ggplot(t2, aes(x = Var1, y = Freq, fill = Var2, group = 1)) +
  geom_bar(stat = 'identity', position = 'stack') +
  facet_grid(~ Var2) +
  geom_smooth(method = "lm") + 
  xlab("# of seen trials") + theme(legend.position='none') + 
  ylab("# of question sequences") + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.text.x = element_text(size = 5)) + 
  theme(axis.text.y = element_text(size = 8)) + 
  scale_fill_brewer(palette="Set1")
p3


########## QUESTIONS: CORRELATION ANALYSIS

res <- subset(t2[t2$Var2=='resolutive',])
nonres <- subset(t2[t2$Var2=='non-resolutive',])

# correlation between resolutive questions and seen trials
res$Var1 <- as.numeric(res$Var1)
cor.test(res$Var1, res$Freq, method=c("spearman"))
# IN THE PAPER 

# correlation between non-resolutive questions and seen trials
nonres$Var1 <- as.numeric(nonres$Var1)
cor.test(nonres$Var1, nonres$Freq, method=c("spearman"))
# NOT IN THE PAPER

# correlation between all questions and seen trials
cor.test(nonres$Var1, nonres$Freq+res$Freq, method=c("spearman"))
# IN THE PAPER

# correlation between % of resolutive questions over total and seen trials
cor.test(res$Var1, res$Freq/(nonres$Freq+res$Freq), method=c("spearman"))
# IN THE PAPER
