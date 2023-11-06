#               Dr. Htet Aung Htut
#             Short course in R for ABT students
# Create two vectors
data1 <- c(50, 46, 54, 55, 55, 52, 53, 51, 47, 49, 51, 55)
data2 <- c(54, 51, 53, 49, 59, 54, 60, 56, 53, 54, 49, 61)
#
sumData <- data1 + data2  # Create new combined vector
print(sumData)
#
# Write output to a file
write.csv(sumData, "D:/statistics/For ABT_DrHAH/My notes/Export/000sum.csv", row.names = FALSE)
read.csv("D:/statistics/For ABT_DrHAH/My notes/Export/000sum.csv")
#
# What happens if row.names set to TRUE
write.csv(sumData, "D:/statistics/For ABT_DrHAH/My notes/Export/000sum.csv", row.names = TRUE)
read.csv("D:/statistics/For ABT_DrHAH/My notes/Export/000sum.csv")

# Control digits in print output
x <- 10
y <- 3
z <- x/y
#
print(z)                 # Default number of digits (7)
print(z, digits = 4)     # Set number of digits to 4
print(z, digits = 11)    # Set number of digits to 11
#
# Rounding
round(3.333333, digits = 2)   # Round a number to two digits
round(5.555555, digits = 3)   # Round a number to three digits
#
# 
signif(6.666666, digits = 4)  # Retain 4 digits only

# Display all R's built-in data sets
data()
#
# Display individual data sets (examples)
mtcars
iris
CO2
trees
uspop
USArrests
anscombe
Titanic
AirPassengers
#
# Check contents of mtcars data set
names(mtcars)
head (mtcars)
nrow(mtcars)
