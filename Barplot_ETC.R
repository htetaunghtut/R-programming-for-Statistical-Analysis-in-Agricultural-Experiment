#         Dr. Htet Aung Htut
#         Short course in R for ABT students
data("ToothGrowth")
tg <- ToothGrowth
head(tg)
library(ggplot2)
library(Rmisc)
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
tgc <- summarySE(tg, measurevar="len", groupvars=c("supp","dose"))
tgc
#Line graphs
# Standard error of the mean
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point()

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)

#A finished graph with error bars representing the standard error of the mean might look like this
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Dose (mg)") +
  ylab("Tooth length") +
  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                   breaks=c("OJ", "VC"),
                   labels=c("Orange juice", "Ascorbic acid"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right
  

#Bar graphs
# Use dose as a factor rather than numeric
tgc2 <- tgc
tgc2$dose <- factor(tgc2$dose)

# Error bars represent standard error of the mean
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


# Use 95% confidence intervals instead of SEM
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
#A finished graph might look like this
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Dose (mg)") +
  ylab("Tooth length") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) +
  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()
  #theme_classic() +scale_fill_manual(values=c('#999999','#E69F00'))
##
##
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(position=position_dodge(.9), colour="black", stat="identity") +
  geom_errorbar(position=position_dodge(.9), width=.25, aes(ymin=len-se, ymax=len+se)) +
  xlab("Dose (mg)") +
  ylab("Tooth length") +
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()  
##
##
# loading the appropriate libraries
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
#load ETC data file
setwd("")
ETC <- read_csv("ETC.csv")
head(ETC)
str(ETC)
#Creating a simple plot for data visualisation
# building a simple plot for data visualisation
qplot(x = Temp, y = Callus, geom = "point", data = ETC) +
  facet_grid(.~Explant, labeller = label_both)
#Analysis of variance for two factors – Two-Way ANOVA
# creating a variable as factor for the ANOVA
ETC$Explant <- as.factor(ETC$Explant)
ETC$Temp_Factor <- as.factor(ETC$Temp)
str(ETC)
# analysis of variance
anova <- aov(Callus ~ Explant*Temp_Factor, data = ETC)
summary(anova)
#library(MASS)
#Creating a table with factors, means and standard deviation
# table with factors, means and standard deviation
str(ETC)
mean(ETC$Callus)
sd (ETC$Callus)  
a<-ETC %>%
  group_by(Explant, Temp)

b<- ETC%>% 
  group_by(Temp) %>%
  summarise(
    mean = mean(Callus), sd = sd(Callus)
  ) %>%
  arrange(desc(mean))
c <- ETC %>%
  group_by(Explant) %>%
  summarise(
    mean = mean (Callus), sd = sd (Callus)
    ) %>%
  arrange(desc(mean))
  
  
data_summary <- ETC %>%
  group_by(Explant, Temp) %>%
  summarize(
    mean = mean(Callus), 
    sd = sd(Callus)
    ) %>%
  arrange(desc(mean))

data_summary

#Comparing means by Tukey’s test
# Tukey's test
tukey <- TukeyHSD(anova)
print(tukey)
#Compact letter display to indicate significant differences
# creating the compact letter display
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)
cld <- as.data.frame.list(tukey.cld$`Explant:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)
write.csv(data_summary,"D:/statistics/For ABT_DrHAH/Bar plot test/ETC_summary.csv")
