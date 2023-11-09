library(readxl)
#lm() function
str (Gomez_CRD)
View(Gomez_CRD)
model <- lm (Gomez_CRD$yield~Gomez_CRD$trt)
summary <- summary(model)
summary
anova <-anova(model)
anova
par(mfrow=c(1,2))
plot(model, which=1)
plot(model, which=2)
library(agricolae)
LSD <-LSD.test(Gomez_CRD$yield,Gomez_CRD$trt,anova$`Df`[2],anova$`Mean Sq`[2])
LSD
#
#aov() fucntion
#Check your data
# Show a random sample
set.seed(1234)
dplyr::sample_n(Gomez_CRD, 10)
#Compute summary statistics by groups - count, mean, sd:
library(dplyr)
group_by(Gomez_CRD, trt) %>%
  summarise(
    count = n(),
    mean = mean(yield, na.rm = TRUE),
    sd = sd(yield, na.rm = TRUE)
  )
#Visualize your data
# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")
library(devtools)
library(ggpubr)
library(ggplot2)
# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggpubr")
ggboxplot(Gomez_CRD, x = "trt", y = "yield", 
          color = "trt",
          ylab = "Yield (kg/ha)", xlab = "Treatment")

Insecticidesplot<-ggboxplot(Gomez_CRD, x = "trt", y = "yield", 
                            color = "trt",
                            ylab = "Yield (kg/ha)", xlab = "Insecticides dosage (kg/ha)")
print(Insecticidesplot + ggtitle("Grain yield of rice in insecticides application"))
print(Insecticidesplot + labs(y = "Grain yield (kg/ha)", x = "Insecticides dosage (kg/ha)"))
print(Insecticidesplot + labs(colour = "Insecticides"))

Insecticidesplot+theme(axis.text.x = element_text(face="bold", color="#993333", 
                                                  size=14, angle=45),
                       axis.text.y = element_text(face="bold", color="#993333", 
                                                  size=14, angle=45)) 
Insecticidesplot+theme(axis.text.x = element_text(face = "bold",color="purple", 
                                                  size=7, angle=20),
                       axis.text.y = element_text(face = "bold",color="purple", 
                                                  size=7, angle=20)) 

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
#Visualize your data with ggpubr:
library("ggpubr")
ggline(Gomez_CRD, x = "trt", y = "yield", 
       add = c("mean_se", "jitter"), 
        ylab = "Yield (kg/ha)", xlab = "Insecticides")
# Box plot
boxplot(yield ~ trt, data = Gomez_CRD,
        xlab = "Insecticides", ylab = "Yield",
        frame = FALSE)
p <- ggplot(Gomez_CRD, aes(x=trt, y=yield)) + 
  geom_boxplot()
print(p)
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(Gomez_CRD, aes(x=trt, y=yield)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(Gomez_CRD, aes(x=trt, y=yield)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

# Box plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=4)

#Box plot with dots
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))

# Change box plot line colors by groups
p<-ggplot(Gomez_CRD, aes(x=trt, y=yield, color=trt)) +
  geom_boxplot()
p
# Use custom color palettes
#p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9",??))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()

#Change box plot fill colors
# Use single color
ggplot(Gomez_CRD, aes(x=trt, y=yield)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
# Change box plot colors by groups
p<-ggplot(Gomez_CRD, aes(x=trt, y=yield, fill=trt)) +
  geom_boxplot()
p

# Use custom color palettes
#p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9",??))
# use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
# Use grey scale
p + scale_fill_grey() + theme_classic()

#Change the legend position
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend

#Change the order of items in the legend
p + scale_x_discrete(breaks=c('Azodrin', 'DDT + BHC', 'Dimecron-Boom', 'Dimecron-Knap', 'Dol-Mix (1 kg)', 'Dol-Mix (2 kg)', 'Control'))
#
p + scale_fill_discrete(breaks=c('Azodrin', 'DDT + BHC', 'Dimecron-Boom', 'Dimecron-Knap', 'Dol-Mix (1 kg)', 'Dol-Mix (2 kg)', 'Control'))
#
p + scale_fill_discrete(limits=c('Azodrin', 'DDT + BHC', 'Dimecron-Boom', 'Dimecron-Knap', 'Dol-Mix (1 kg)', 'Dol-Mix (2 kg)', 'Control'))
#You can use dplyr::mutate function to change automatically ascending the legend

#Box plot with multiple groups
# Change box plot colors by groups
#Load the new supplemental data, Gomez_CRD_supp
str(Gomez_CRD_supp)
ggplot(Gomez_CRD_supp, aes(x=trt, y=yield, fill=supp)) +
  geom_boxplot()
# Change the position
p1<-ggplot(Gomez_CRD_supp, aes(x=trt, y=yield, fill=supp)) +
  geom_boxplot(position=position_dodge(1))
p1
# Add dots
p1 + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))

#Customized box plots
# Basic box plot
ggplot(Gomez_CRD, aes(x=yield, y=trt)) + 
  geom_boxplot(fill="gray")+
  labs(title="Plot of insecticides application",x="Insecticides (kg/ha)", y = "Yield (kg/ha)")+
  theme_classic()
# Change  automatically color by groups
bp <- ggplot(Gomez_CRD, aes(x=trt, y=yield, fill=trt)) + 
  geom_boxplot()+
  labs(title="Plot of insecticides application",x="Insecticides (kg/ha)", y = "Yield (kg/ha)")
bp + theme_classic()

# Continuous colors
bp + scale_fill_brewer(palette="Blues") + theme_classic()
# Discrete colors
bp + scale_fill_brewer(palette="Dark2") + theme_minimal()
# Gradient colors
bp + scale_fill_brewer(palette="RdBu") + theme_minimal()

# Change colors
#p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9",??))

#Remove slashes in the legend of a bar plot

#load the new data Gomez_CRD_supp1
str(Gomez_CRD_supp1)
# Default plot
ggplot(data=Gomez_CRD_supp1, aes(x=supp1, fill=supp1)) + geom_bar()
# Change bar plot border color, 
# but slashes are added in the legend
ggplot(data=Gomez_CRD_supp1, aes(x=supp1, fill=supp1)) +
  geom_bar(colour="black")
# Hide the slashes: 
#1. plot the bars with no border color,
#2. plot the bars again with border color, but with a blank legend.
ggplot(data=Gomez_CRD_supp1, aes(x=supp1, fill=supp1))+ 
  geom_bar() + 
  geom_bar(colour="black", show_guide=FALSE)

# plotmeans
library("gplots")
plotmeans(yield ~ trt, data = Gomez_CRD, frame = FALSE,
          xlab = "Insecticides", ylab = "Yield (kg/ha)",
          main="Mean Plot with 95% CI") 
# Compute the analysis of variance
res.aov <- aov(yield ~ trt, data = Gomez_CRD)
# Summary of the analysis
summary(res.aov)
##As the p-value is less than the significance level 0.001, 
#we can conclude that there are significant differences between the groups highlighted with “***" in the model summary.
TukeyHSD(res.aov)
##The difference between Control and Azodrin is significant with an adjusted p-value of 0.017, and
#DDT + BHC-Control, Dol-Mix (1 kg)-Control, Dol-Mix (2 kg)-Control, etc.

#library(multcomp)
#Pairewise t-test
pairwise.t.test(Gomez_CRD$yield, Gomez_CRD$trt,
                p.adjust.method = "BH")
# 1. Homogeneity of variances
plot(res.aov, 1)
#Points 15, 7, 5 are detected as outliers, which can severely affect normality and homogeneity of variance. 
#It can be useful to remove outliers to meet the test assumptions.
library(car)
leveneTest(yield ~ trt, data = Gomez_CRD)
#The p-value is not less than the significance level of 0.05
#No evidence to suggest that the variance across groups is statistically significantly different.
#Therefore, we can assume the homogeneity of variances in the different treatment groups

#Relaxing the homogeneity of variance assumption
#ANOVA test with no assumption of equal variances
oneway.test(yield ~ trt, data = Gomez_CRD)
pairwise.t.test(Gomez_CRD$yield, Gomez_CRD$trt,
                p.adjust.method = "BH", pool.sd = FALSE)

##Check the normality assumption
# 2. Normality
plot(res.aov, 2)
##As all the points fall approximately along this reference line, we can assume normality

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
#Shapiro-Wilk test on the ANOVA residuals (W = 0.96, p = 0.9) which finds no indication that normality is violated.

#Non-parametric alternative to one-way ANOVA test
kruskal.test(yield ~ trt, data = Gomez_CRD)
##non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test, which can be used when ANNOVA assumptions are not met
