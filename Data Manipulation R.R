#                   Dr. Htet Aung Htut
#                   Short course in R

library(readxl)
petunia <- read.csv(file = "D:/statistics/For ABT_DrHAH/Short traing course/Data/flower.csv", header = TRUE, sep = ",")
str (petunia)
# Quick checks on Petunia data
names(petunia)        # Display all variable names
head(petunia)         # Display first six rows
tail(petunia)         # Display last six rows
nrow(petunia)         # Display number of rows
#
# Display each variable on its own
petunia$treat
petunia$nitrogen
petunia$block
petunia$height
petunia$weight
petunia$leafarea
petunia$shootarea
#
#
# Create subset of petunia treatment in tip only
petuniatip <- subset(petunia, treat == "tip")
petuniatip
nrow(petuniatip)
# Create subset of petunia treatment in notip only
petunianotip <- subset(petunia, treat == "notip")
petunianotip
nrow(petunianotip)
#
# Create subset of petunia nitrogen in low only
petunianitl <- subset(petunia, nitrogen == "low")
petunianitl
nrow(petunianitl)
#
# Create subset of petunia nitrogen in medium only
petunianitm <- subset(petunia, nitrogen == "medium")
petunianitm
nrow(petunianitm)
#
# Create subset of petunia nitrogen in high only
petunianith <- subset(petunia, nitrogen == "high")
petunianith
nrow(petunianith)
# Perform calculation on a subset only
mean(petuniatip$leafarea)
mean(petunianotip$leafarea)
mean(petunianitl$leafarea)
mean(petunianitm$leafarea)
mean(petunianith$leafarea)

Gomezcrd<-read.csv("D:/statistics/My Git-repository/Gomez-CRD.csv",header = TRUE, sep = ",")
# Access the data in 'leafarea' column
# dollar sign is used
Gomezcrd$yield

library(ggplot2)
# Load the data
attach (Gomezcrd)
# Convert height to a factor variable
Gomezcrd$yield <- as.factor(Gomezcrd$trt)
# Print a sample of the data
head(Gomezcrd)
str(Gomezcrd)
Gomezcrd$trt<-factor(Gomezcrd$trt)
ggplot(Gomezcrd, aes(x = factor(trt), y = yield)) +
  geom_point()

# Basic plot
plot(petunia)
p <- ggplot(petunia, aes(x = flowers, fill = nitrogen))
# Bar plot
p + geom_bar(stat = "bin")
# Area plot
p + geom_area(stat = "bin")

## A scatter plot 
head(petunia)
# Change the point size, and shape
ggplot(petuniatip, aes(x=height, y=weight)) +
  geom_point(size=2, shape=23)
ggplot(petuniatip, aes(x=height, y=weight)) +
  geom_point(size=1.5, shape=18)
ggplot(data = petuniatip, aes(x = height, y = weight)) +
  geom_point() + # to draw points
  geom_line() # to draw a line
ggplot(data = petuniatip, aes(x = height, y = weight)) +
  geom_point() + # to draw points
  geom_line(data = head(petuniatip), color = "red")
ggplot(data = petuniatip, aes_string(x = "height", y = "weight")) +
  geom_point(color = "red") +
  geom_smooth()
# Log2 transformation in the aes()
ggplot(data = petuniatip, aes(x = log2(height), y = log2(weight))) +
  geom_point() +
  geom_smooth(method=lm)

ggpoints <- function (data, xName, yName){
  p <- ggplot(data = data, aes_string(xName, yName)) +
    geom_point(color = "red") +
    geom_smooth()
  return(p)
}
ggpoints(petuniatip, xName ="height", yName = "weight")

# Add the regression line
ggplot(petuniatip, aes(x=height, y=weight)) + 
  geom_point()+
  geom_smooth(method=lm)
ggplot(petunianotip, aes(x=height, y=weight)) + 
  geom_point()+
  geom_smooth(method=lm)
# Remove the confidence interval
ggplot(petuniatip, aes(x=height, y=weight)) + 
  geom_point()+
  geom_smooth(method=lm, se=FALSE)
# Loess method
ggplot(petuniatip, aes(x=height, y=weight)) + 
  geom_point()+
  geom_smooth()
# Change the point colors and shapes
# Change the line type and color
ggplot(petuniatip, aes(x=height, y=weight)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")
# Change the confidence interval fill color
ggplot(petuniatip, aes(x=height, y=weight)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")
# Change point shapes by the levels of nitrogen
ggplot(petuniatip, aes(x=nitrogen, y=height, shape=nitrogen)) +
  geom_point()
# Change point shapes and colors
ggplot(petuniatip, aes(x=nitrogen, y=height, shape=nitrogen, color=nitrogen)) +
  geom_point()
# Change point shapes, colors and sizes
ggplot(petuniatip, aes(x=nitrogen, y=height, shape=nitrogen, color=nitrogen, size=nitrogen)) +
  geom_point()
# Change point shapes and colors manually
ggplot(petuniatip, aes(x=nitrogen, y=height, color=nitrogen, shape=nitrogen)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme(legend.position="top")
# Change the point sizes manually
ggplot(petuniatip, aes(x=nitrogen, y=weight, color=nitrogen, shape=nitrogen))+
  geom_point(aes(size=nitrogen)) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(2,3,4))+
  theme(legend.position="top")
p <- ggplot(petuniatip, aes(x=nitrogen, y=weight, color=nitrogen, shape=nitrogen)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  theme_classic()
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey()
# Add marginal rugs
ggplot(petuniatip, aes(x=nitrogen, y=weight)) +
  geom_point() + geom_rug()
# Change colors
ggplot(petuniatip, aes(x=nitrogen, y=weight, color=nitrogen)) +
  geom_point() + geom_rug()
## looking at a linear fit, we see it is poor at the extremes
p <- ggplot(petuniatip, aes(x = height, y = weight)) +
  geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, size = 1)
p + stat_smooth(method = "loess", formula = y ~ x, size = 1)
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)
## R can automatically create these using the poly() function
p + stat_smooth(method = "lm", formula = y ~ poly(x, 2), size = 1)
library(mgcv)
str (petuniatip)
## when vs is mapped to colour, separate lines are automatically fit
ggplot(petuniatip, aes(x = leafarea, y = flowers)) + geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE)
#qplot to ggplot
#qplot(leafarea, flowers, data = petuniatip, color = leafarea)
ggplot(petuniatip, aes (x = height, y = leafarea)) + 
  geom_point(aes(colour = height))
ggplot(petuniatip, aes (x = height, y = leafarea, z = nitrogen)) + 
  geom_point(aes(colour = nitrogen))
# Change color and shape by groups (factor)
petuniatip$cyl <- factor(petuniatip$nitrogen)
ggplot(petuniatip, aes (x = leafarea, y = height, z = nitrogen)) + 
  geom_point(aes(shape = nitrogen, colour = nitrogen))
#Bar Plots of Counts
ggplot(petunia, aes(flowers)) +
  geom_bar(fill = "steelblue")+ theme_minimal()
#
