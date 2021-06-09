# LOAD LIBRARIES

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
# 1. add descriptive statistics table


######################
######################
########EXP1##########
######################
######################

# SET MY WORKING DIRECTORY
mypath <- "../aggregated_data/"
setwd(mypath)

# LOAD DATA FROM .CSV
dataEXP1 <- read.csv("exp1_results.csv")
head(dataEXP1)
sapply(dataEXP1,class)

# FORMAT FACTORIAL VARIABLES TO FACTOR
dataEXP1$dft <- as.factor(dataEXP1$dft)
dataEXP1$accuracy <- as.factor(dataEXP1$accuracy)
sapply(dataEXP1,class)

####### MAXIMUM INTER-SPEAKER AGREEMENT
tabexp1<-data.frame(dataEXP1$partsize,dataEXP1$trial,dataEXP1$class)
table(tabexp1)
myt <- data.frame(table(tabexp1))   
myT <- myt[myt$dataEXP1.class=="borderline", ]
bord <- head(myT,20)
bord
myT2 <- myt[myt$dataEXP1.class=="clearcut", ]
cc <- tail(myT2,20)
cc

bordB <- bord[bord$dataEXP1.partsize=='BIG',]
bordBvalues <- bordB$Freq/20
bordBvalues

bordAGR <- replace(bordBvalues, bordBvalues==0.35, 0.65)
# TODO: less hardcoded
mean(bordAGR)
sd(bordAGR)
median(bordAGR)

ccB <- cc[cc$dataEXP1.partsize=='BIG',]
ccBvalues <- ccB$Freq/20
ccBvalues

ccAGR <- replace(ccBvalues, ccBvalues==0.05, 0.95)
ccAGR <- replace(ccAGR, ccAGR==0.00, 1.00)
ccAGR <- replace(ccAGR, ccAGR==0.15, 0.85)
ccAGR <- replace(ccAGR, ccAGR==0.35, 0.65)
# TODO: less hardcoded
mean(ccAGR)
sd(ccAGR)
median(ccAGR)

# Combine into one long data frame
a <- data.frame(group = "borderline", value = bordAGR)
b <- data.frame(group = "clear-cut", value = ccAGR)


# BOXPLOT EXP1 (FIG 3A)
plot.data <- rbind(a, b)
plot.data

p <- ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() +
  theme(legend.position = "top") + xlab("") +
  ylab("maximum inter-speaker agreement") +
  # ylab("participants-malevic alignment") +
  scale_y_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.2,1)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  annotate("text", x = "clear-cut", y = 0.5, label = "chance", vjust = -0.5)
p


# TEST NORMALITY of DISTRIBUTION
shapiro.test(ccAGR)
# non-normal
shapiro.test(bordAGR)
# pseudo-normal


###### ONE-SIDED WILCOXON TEST

# PARAMETRIC & NON-PARAMETRIC TESTS

# t.test(bordAGR,ccAGR,alternative = c("less"))
# NOT IN THE PAPER

# wilcox.test(bordAGR,ccAGR, alternative="less")
# NOT IN THE PAPER

wilcox.test(ccAGR,bordAGR, alternative="greater")
# THIS IS IN THE PAPER


##### PARTICIPANTS' ALIGNMENT WITH MALEVIC
tabexp1B<-data.frame(dataEXP1$trial,dataEXP1$accuracy)
table(tabexp1B)
mytt <- data.frame(table(tabexp1B)) 

align <- tail(mytt,20)

alignB <- head(align,10)
alB <- alignB[alignB$dataEXP1.accuracy=='1',]
BB <- alB$Freq/20
alignC <- tail(align,10)
alC <- alignC[alignC$dataEXP1.accuracy=='1',]
alC$Freq/20
CC <- alC$Freq/20

# BORDERLINE ALIGNMENT
mean(BB)
sd(BB)
median(BB)

# CLEARCUT ALIGNMENT
mean(CC)
sd(CC)
median(CC)


# Combine into one long data frame
a2 <- data.frame(group = "borderline", value = BB)
b2 <- data.frame(group = "clear-cut", value = CC)


# BOXPLOT EXP1 (FIG 3A)
plot.data2 <- rbind(a2, b2)
plot.data2

p2 <- ggplot(plot.data2, aes(x=group, y=value, fill=group)) + geom_boxplot() +
  theme(legend.position = "top") + xlab("") +
  ylab("participants-malevic alignment") +
  scale_y_continuous(breaks=c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), limits=c(0.2,1)) +
  geom_hline(yintercept=0.5, linetype="dashed", color = "black") +
  annotate("text", x = "clear-cut", y = 0.5, label = "chance", vjust = -0.5)
p2


####### TEST NORMALITY of DISTRIBUTION
shapiro.test(CC)
# non-normal
shapiro.test(BB)
# normal-like


####### PARAMETRIC AND NON-PARAMETRIC TESTS

# TEST IF GREATER THAN CHANCE, I.E, 0.5 (NO)
t.test(BB, mu = 0.5)
# borderline, normally-distributed

# TEST IF GREATER THAN CHANCE, I.E, 0.5 (YES)
wilcox.test(CC, mu = 0.5)
# clearcut, non-normally distributed