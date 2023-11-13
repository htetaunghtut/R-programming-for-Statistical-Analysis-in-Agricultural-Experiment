##          Dr. Htet Aung Htut
##          Short course in R for ABT students

#Step 1: Load Libraries
require(tidyverse)
require(mosaic)
library(ggplot2)
library(emmeans)
library(doBy)
library(lmerTest)
library(multcompView)
#Step 2: Import Data
setwd("")
str(Gomez_RCB_loc)
block = as.factor(Gomez_RCB_loc$blk)
location = as.factor(Gomez_RCB_loc$location)
#Step 3: Check and update data
head (Gomez_RCB_loc)
tail(Gomez_RCB_loc)
nrow(Gomez_RCB_loc)
summary(Gomez_RCB_loc)
favstats(yield~trt,data=Gomez_RCB_loc)
favstats(yield~block,data=Gomez_RCB_loc)
#Step 4. Explore data
ggplot(Gomez_RCB_loc,aes(x=trt,y=yield)) + geom_boxplot()
ggplot(Gomez_RCB_loc,aes(x=trt,y=yield)) + stat_summary()
#
ggplot(data= Gomez_RCB_loc,aes(y=yield,x=trt,col= block))+
  geom_point()
summaryBy(yield ~ trt, data= Gomez_RCB_loc, FUN=c(mean,median,sd))

#Step 5. Specify a model for data
#CRD model
CRD.model <- aov(yield~trt,data=Gomez_RCB_loc)
anova(CRD.model)
# set up model
CRD1 <-lm (Gomez_RCB_loc$yield~Gomez_RCB_loc$trt)
summary <- summary(CRD1)
summary
anova <-anova(CRD1)
anova
par(mfrow=c(1,2))
plot(CRD1, which=1)
plot(CRD1, which=2)
#
#RCB model
RBD.model <- aov(yield~trt+block,data=Gomez_RCB_loc)
anova(RBD.model)
TukeyHSD(RBD.model)
#set up model
RBD1 <-lmer(yield~trt+(1|block), data=Gomez_RCB_loc)
summary(RBD1)
#RCB two way
attach(Gomez_RCB_loc)
##
RBD2way <- aov (yield ~ trt*location , data = Gomez_RCB_loc)
summary(RBD2way)
#Step 6. Check the model
plot(RBD1)
qqnorm(resid(RBD1))
qqline(resid(RBD1))
#Step 7. Interpret the model
anova(RBD1, ddf="Kenward-Roger")
print(VarCorr(RBD1), comp=("Variance"))
#Step 8. Present the results from the model
emmip(RBD1,~trt,CIs = TRUE)
emmeans(RBD1, ~ trt)
#
library(agricolae)
data("PlantGrowth")
plant.lm <- lm(weight ~ group, data = PlantGrowth)
plant.av <- aov(plant.lm)
summary(plant.av)
tukey.test <- TukeyHSD(plant.av)
tukey.test
plot(tukey.test)
tukey.test2 <- HSD.test(plant.av, trt = 'group')
tukey.test2


data("warpbreaks")
warp.lm <- lm(breaks ~ wool*tension, data = warpbreaks)
warp.emm <- emmeans(warp.lm, ~ tension | wool)
contrast(warp.emm, "poly")
contrast(warp.emm, "trt.vs.ctrl", ref = "M")
## Not run: 
## Same when enhanced labeling is used:
contrast(warp.emm, "trt.vs.ctrl", 
         enhance.levels = "tension", ref = "tensionM")
## End(Not run)

# Comparisons with grand mean
contrast(warp.emm, "eff")
# Comparisons with a weighted grand mean
contrast(warp.emm, "eff", wts = c(2, 5, 3))

# Compare only low and high tensions
# Note pairs(emm, ...) calls contrast(emm, "pairwise", ...)
pairs(warp.emm, exclude = 2)
# (same results using exclude = "M" or include = c("L","H") or include = c(1,3))

### Setting up a custom contrast function
helmert.emmc <- function(levs, ...) {
  M <- as.data.frame(contr.helmert(levs))
  names(M) <- paste(levs[-1],"vs earlier")
  attr(M, "desc") <- "Helmert contrasts"
  M
}
contrast(warp.emm, "helmert")
## Not run: 
# See what is used for polynomial contrasts with 6 levels
emmeans:::poly.emmc(1:6)



  