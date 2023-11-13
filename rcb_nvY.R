##        Dr. Htet Aung Htut
##        Short course in R for ABT students



#load the data file
setwd("rcb-nvY.xlsx file")
str(rcb_nvY)
summary(rcb_nvY)
#Create a histogram 

hist(rcb_nvY$yield, xlim=c(0, 10),ylab = "Yield (kg/ha)")
#boxplot

boxplot(yield~nitrogen,data=rcb_nvY, xlab="Nitrogen fertilizer", ylab="Yield (kg/ha)")
title("Boxplot of crop yield variation due to Nitrogen application")
#boxplot

boxplot(yield~var,data=rcb_nvY, xlab="Rice variety", ylab="Yield (kg/ha)")
title("Boxplot of crop yield variation due to genotype")

#Define a dataframe

Y<-rcb_nvY

#Summary

summary(Y)
# Factor assignment

rcb_nvY$nitrogen=as.factor(rcb_nvY$nitrogen)

rcb_nvY$var=as.factor(rcb_nvY$var)

#Analysis of variance for both the factors: Nitrogen fertilizer and variety taken separately
model1=aov(yield~nitrogen,data=rcb_nvY) 
anova(model1)

model2=aov(yield~var,data=rcb_nvY) 
anova(model2)

model12=aov(yield~nitrogen*var,data=rcb_nvY) 
anova(model12)

#Analysis of variance for the factor: Nitrogen fertilizer and variety within each rep 
model123=aov(yield~nitrogen+rep,data=rcb_nvY) 
anova(model123)

model124=aov(yield~var+rep,data=rcb_nvY) 
anova(model124)

model125=aov(yield~(nitrogen*var)+ rep,data=rcb_nvY) 
anova(model125)

#Interaction Plot
interaction.plot(rcb_nvY$nitrogen, rcb_nvY$var,rcb_nvY$yield, xlab= "Nitrogen Fertilizer", ylab="yield", main ="Interaction plot", trace.label="var")


#Diagnostics/Model Adequacy Checking
# Shapiro Test

shapiro.test(rcb_nvY$yield)
#Plot

qqnorm(rcb_nvY$yield)
qqline(rcb_nvY$yield)
# Diagnostics check for the model with nitrogen factor

qqnorm(residuals(model123))
qqline(residuals(model123))
plot(fitted(model123),residuals(model123))

#Diagnostics for the variety factor

qqnorm(residuals(model124))
qqline(residuals(model124))
plot(fitted(model124),residuals(model124))

#Diagnostics for the interaction effect

qqnorm(residuals(model125))
qqline(residuals(model125))
plot(fitted(model125),residuals(model125))

#Tukey’s test
#nitrogen and rep
TukeyHSD(model123, ordered = FALSE, conf.level = 0.95)
#var and rep
TukeyHSD(model124, ordered = FALSE, conf.level = 0.95)
#All factors interaction
TukeyHSD(model125, ordered = FALSE, conf.level = 0.95)
# Plot the results
m1 = TukeyHSD(model123, which="nitrogen", ordered = FALSE)
m2 = TukeyHSD(model124, which="var",ordered = FALSE)
m3= TukeyHSD(model125, which="var",ordered = FALSE)
plot(m1)
plot(m2)
plot(m3)
