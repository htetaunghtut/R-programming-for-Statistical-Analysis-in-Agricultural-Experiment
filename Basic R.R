#               Author : Dr. Htet Aung Htut
#               
#
"Hello World"
print("Hello World")

"မြန်မာနိုင်ငံ"
print("မြန်မာနိုင်ငံ")

# Create two simple vectors
x <- 5
y <- 10
#
# Compare x and y using operators
x == y     # FALSE
x != y     # TRUE
x < y      # TRUE
x > y      # FALSE
x <= y     # TRUE
x >= y     # FALSE
#
# Compare entire vectors at once
a <- c(10, 20, 30)
b <- c(10, 25, 30)
#
a == b     # FALSE
a != b     # TRUE
a < b      # TRUE
a > b      # FALSE
a <= b     # TRUE
a >= b     # FALSE
# Assign text to a variable
a <- c ("ရေဆင်းစိုက်ပျိုးရေးတက္ကသိုလ်", "ဇီဝနည်းပညာ")
a
print (a)
b = c ("သင်ကြားရေး", "သုတေသန")
b
print(b)
# Add two vectors
v1 + v2
#
c(v1, v2)   # combine two vectors
#
# Create a vector with text
v3 <- c("ငါ", "မင်းကို" , "သတိရတယ်")
print(v3)
#
# The seq function
seq(1, 10)                      # simple version
seq(from = 1, to = 10)          # same as above clearer syntax
seq(from = 1, to = 10, by = 2)  # every second number
seq(from = 0, to = 20, by = 5)
seq(from = 1, to = 2, length.out = 5)
#
# Store sequence in a vector
mySeq <- seq(from = 1, to = 100)
print(mySeq)
#
# Some useful sequences
seq(from = 1900, to = 1999)           # every year 20th century
seq(from = 1900, to = 1999, by = 10)  # Each decade
seq(from = 0, to = 100)               # Percent values
#
# Create a series of repeated values
rep(1, times = 5)    # Replicate a number
rep(1:10, 2)         # Replicate a sequence
rep(1:10, each = 2)  # Replicate elements by "each" number of times
# Repeat text
rep("Hello", 10)
#
# Try to combine text and numeric Vectors
c(v1, v2, v3)
#
# Assign a new value to x
x <- 10
print (x)
y <- 20
print (y)
z <- 15
print (z)
# Add variables and display result
c <- x + y + z
print (c)
# Other assignment operators
u = 25
u
50 -> v
v
# Display sum of u and v
print (u+v)
# Create a simple vector using combine function
v1 <- c(1, 2,3)
print(v1)
#
v2 <- c(4, 5, 6)
print(v2)
#
# Create two vectors (Biotech group)
age <- c(40, 30, 26, 28, 27)
names(age) <- c("ထက်အောင်", "ဆုရည်", "ပိုင်ဖြိုးမင်း","ယမင်း","အိမ့်")
print(age)
#
age [1]          # Select by number
#
age ["ထက်အောင်"]   # What is the age of ထက်အောင် ?
age ["ပိုင်ဖြိုးမင်း"]  # What is the age of ပိုင်ဖြိုးမင်း?
# Select two or more elements by name
age [c("ယမင်း", "အိမ့်")]
#
# Names not listed
age ["နရဏီ"]  # What is the age of နရဏီ? (returns NA)

# Ask user for their name
name <- readline(prompt = "Htet Aung Htut: ")
#
# Ask user for their age
age <- c (40)
age
#
# Convert age to an integer
age_new <- gsub(",", "", age)    # Applying gsub function
age_new                          # Print updated example vector
# Output a message to the user
print(paste("Hi", "Htet Aung Htut", "next year you will be", 
            age + 1, "years old."))

# Calculate Body Mass Index (BMI)
# Formula: BMI = Weight (kg) / Height (m) x Height (m)
height <-  1.82  # metres
weight <- 96.5   # kilograms
#
bmi <- weight/(height * height) # formula
print(bmi)
#
#Check your data whtether numeric or not
x <- 10.5               # Decimal value
print(x)
# Check Data Type
class(x)
# Check if data type is numeric
is.numeric(x)
#
# Check natural numbers
y <- 10                 # No decimal
is.numeric(y)           # also numeric
#
x <- 10
print(x)
#
#Change your data type numeric to integer
# Check data type
class(x)                # numeric
#
# Check if data type is integer
is.integer(x)           # False
#
# Change numeric variable to an integer
x <- as.integer(x)
#
# Check data type again
class(x)                # integer
#
# Check if data type is integer
is.integer(x)           # True

# logical data?
# Create two simple numeric vectors
x <- 1
y <- 2
#
# Compare two vectors and store result in a new vector
z <- x > y
print(z)
#
# Check Data Type
class(z)             # logical
#
# Check if data type is logical
is.logical(z)        # True

# Read in Data
oil <- c(50, 46, 54, 55, 55, 52, 53, 51, 47, 49, 51, 55)
print(oil)
#
gold <- c(1267, 1238, 1157, 1192, 1234, 1231,  
          1267, 1246, 1260, 1237,1283, 1314)
print(gold)
# Calculate the Mean, Median, Standard Deviation, 
# Variance, and Correlation Coefficient of each vector
#
mean (oil)
median (oil) 
sd (oil)
var (oil)
cor (oil, gold)   # Default is Pearson's Correlation Coefficient
#
mean (gold)
median (gold)
sd (gold)
var (gold)
cor (gold, oil)  
#
library(readxl)
petunia <- read.csv(file = "D:/statistics/For ABT_DrHAH/Short traing course/Data/flower.csv", header = TRUE, sep = ",")
#
# Check that data is being read correctly
head(petunia)  # Show first six rows
tail(petunia)  # Show last six rows
nrow(petunia)  # Count number of rows
str(petunia)
summary (petunia)
#
# Display each variable
petunia$height # Show height data only
petunia$weight ## Show weight data only
#
# Can now use data, eg to calculate the mean 
mean(petunia$height)
mean(petunia$weight)
#
# Is there a correlation between the petunia of plant height and weight?
cor(petunia$height, petunia$weight)

#
# Some Basic Statistics (mean and skewness)
pltheight <- c(7.5,10.7,11.2,10.4,10.4,9.8,6.9,9.4,10.4,12.3,10.4,11,
              7.1,6,9,4.5,12.6,10,10,8.5,14.1,10.1,8.5,6.5,11.5,7.7,
              6.4,8.8,9.2,6.2,6.3,17.2,8,8,6.4,7.6,9.7,12.3,9.1,8.9,
              7.4,3.1,7.9,8.8,8.5,5.6,11.5,5.8,5.6,5.3,7.5,4.1,3.5,
              8.5,4.9,2.5,5.4,3.9,5.8,4.5,8,1.8,2.2,3.9,8.5,8.5,6.4,
              1.2,2.6,10.9,7.2,2.1,4.7,5,6.5,2.6,6,9.3,4.6,5.2,3.9,
              2.3,5.2,2.2,4.5,1.8,3,3.7,2.4,5.7,3.7,3.2,3.9,3.3,5.5,4.4)
mean(pltheight)         # works
skewness(pltheight)     # does not work
#
library()   # Check packages already installed
#
# Install 'moments' package
install.packages("moments")
#
# Load 'moments' package
library(moments)
#
# Recalculate skewness
skewness(pltheight)     # works
