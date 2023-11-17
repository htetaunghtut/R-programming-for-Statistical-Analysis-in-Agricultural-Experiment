#         Dr. Htet Aung Htut
#         Short course in R for ABT students

#load the library
library(tidyr)
library(mice)
library(VIM)
misdatrcb <- read.csv("Gomez-RCB-missingdat.csv")
str(misdatrcb)
summary(misdatrcb)
#Missing data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(misdatrcb, 2, p)
#Misdatrcb, trt and blk contains no missing values, but yield contains 3 points in missed
md.pairs(misdatrcb)
#rr indicates how many data points are observed

#rm indicates Observed and Missing

#mr indicates Missing versus observed

#mm indicates Missing versus Missing
marginplot(misdatrcb[,c('trt','yield')])
#Impute
impute <- mice(misdatrcb[,2:3], m=3, seed = 123)
print(impute)
impute$imp$yield
misdatrcb[14,]
#Complete data
newDATA <- complete(impute, 1)
#Distribution of oberserved/imputed values
xyplot(impute, yield ~ blk | .imp, pch = 20, cex=1.4)
