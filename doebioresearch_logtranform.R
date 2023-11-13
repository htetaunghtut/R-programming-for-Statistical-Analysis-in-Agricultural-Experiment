##            Dr. Htet Aung Htut
##            Short course in R for ABT students


library(doebioresearch)
data<-data.frame(Treatments=c("T1","T2","T3","T4","T5","T6","T7","T1","T2","T3","T4","T5","T6",
                              "T7","T1","T2","T3","T4","T5","T6","T7"),
                 yield=c(25,21,21,18,25,28,24,25,24,24,16,21,20,17,16,19,14,15,13,11,25),
                 height=c(130,120,125,135,139,140,145,136,129,135,150,152,140,148,130,135,145,160,145,130,160))
#CRD analysis with LSD test for yield only
crd(data[2],data$Treatments,1)
#CRD analysis with LSD test for both yield and height
crd(data[2:3],data$Treatments,1)
        
data(factorialdata)
#Analysis of Factorial Completely Randomized design along with Dunccan test for Yield only
fcrd2fact(factorialdata[5],factorialdata$Nitrogen,factorialdata$Phosphorus,2)
#Analysis of Factorial Completely Randomized design along with Dunccan test for Yield & Plant Height
fcrd2fact(factorialdata[5:6],factorialdata$Nitrogen,factorialdata$Phosphorus,2)

data("factorialdata")
#Analysis of Factorial Completely Randomized design along with Dunccan test for Yield only
fcrd2fact(factorialdata[5],factorialdata$Nitrogen,factorialdata$Phosphorus,2)
#Analysis of Factorial Completely Randomized design along with Dunccan test for Yield & Plant Height
fcrd2fact(factorialdata[5:6],factorialdata$Nitrogen,factorialdata$Phosphorus,2)

#FRBD analysis along with dunccan test for two dependent var.
frbd3fact(factorialdata[5:6],factorialdata$Replication,factorialdata$Nitrogen,
          factorialdata$Phosphorus,factorialdata$Potassium,2)

data("splitdata")
#Using Date of sowing as Main-plot factor and varieties as sub-plot factor and using LSD test
#Split plot analysis with LSD test for Yield
splitplot(splitdata[4],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
#Split plot analysis with LSD test for both Yield and Plant Height
splitplot(splitdata[4:5],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)

#Split data is used for sake of demonstration
#Using Date of sowing as Column factor and varieties as Row factor and using LSD test for Yield only
stripplot(splitdata[4],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)
#Using Date of sowing as Column factor and varieties as Row factor and using LSD test for both var.
stripplot(splitdata[4:5],splitdata$Replication,splitdata$Date_of_Sowing,splitdata$Varities,1)

#Data transformation
oridat <- read.csv("D:/statistics/My Git-repository/logtransform-gomez.csv")
library(doebioresearch)
larvaeno <- (oridat$larvae_no)
t <-logtransform(larvaeno)
print(t)

#Using dplyr package to transform log10 value
library(dplyr)
log_larvae <- mutate(oridat, loglarvae = log10(larvae_no))
print(log_larvae)
#create histogram for original distribution
hist(oridat$larvae_no, col='steelblue', main='Original')

#create histogram for log-transformed distribution 
hist(log_larvae$loglarvae, col='coral2', main='Log Transformed')

#Exporting data for csv file

write.csv(log_larvae, "D:/statistics/My Git-repository/logtransformed.csv", row.names = FALSE) 
#log_larvae<- read.csv("D:/statistics/My Git-repository/logtransformed.csv")
#str(log_larvae)
logtranformed <- read.csv("D:/statistics/My Git-repository/logtransformed.csv")
str(logtransformed)
library(agricolae)
#RCB model
#You save logtransformed.csv file in directory, now please open the logtransformed.xlsx file for further analysis
#rep = as.factor(log_larvae$rep)
#Mydata
RBD.model <- aov(trnasf_larvae ~ insecticide + block ,data=logtransformed)
anova(RBD.model)
TukeyHSD(RBD.model)
lsd <- LSD.test(RBD.model,"insecticide", p.adj="bonferroni")
print(lsd)
dmrt <- duncan.test(RBD.model,"insecticide", 
                   main="Number of larvae. Dealt with different insecticide")
plot(dmrt,variation="IQR")
duncan.test(RBD.model,"insecticide",alpha=0.01,console=TRUE)
duncan.test(RBD.model,"insecticide",alpha=0.05,console=TRUE)

CRD.model <-lm (logtransformed$trnasf_larvae~logtransformed$insecticide)
summary <- summary(CRD.model)
summary
anova <-anova(CRD.model)
anova
par(mfrow=c(1,2))
plot(CRD.model, which=1)
plot(CRD.model, which=2)

#Gomez data
str (logtransformed)
RBD.model <- aov(gomez_val ~ insecticide + block ,data=logtransformed)
anova(RBD.model)
TukeyHSD(RBD.model)
lsd <- LSD.test(RBD.model,"insecticide", p.adj="bonferroni")
print(lsd)
dmrt <- duncan.test(RBD.model,"insecticide", 
                    main="Number of larvae. Dealt with different insecticide")
plot(dmrt,variation="IQR")
duncan.test(RBD.model,"insecticide",alpha=0.01,console=TRUE)


#Replace on selected olumns
logtransformed["trnasf_larvae"][logtransformed["trnasf_larvae"] == 0] <- NA

RBD.model <- aov(larvae_no ~ insecticide + block ,data=logtransformed)
anova(RBD.model)
TukeyHSD(RBD.model)
lsd <- LSD.test(RBD.model,"insecticide", p.adj="bonferroni")
print(lsd)
dmrt <- duncan.test(RBD.model,"insecticide", 
                    main="Number of larvae. Dealt with different insecticide")
plot(dmrt,variation="IQR")
duncan.test(RBD.model,"insecticide",alpha=0.01,console=TRUE)

