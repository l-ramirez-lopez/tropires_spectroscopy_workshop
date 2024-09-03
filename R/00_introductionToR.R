# Introduction to R and R Studio
# TropiRes workshop 2024

# Start a new script
rm(list = ls()) # remove all objects from the workspace
cat("\014") # clean the console


# Create an R project

# R Studio GUI

# Create a new R script

# R as a calculator -----------------------------------------
1+1

# Assign objects
a <- 2
b <- 3

# Calculate with objects
print(a+b)

# Data types in R ------------------------------------------

# Vector
a <- numeric(10)

# Indexing (Referring to a specific part of a vector)
a[2] <- 5

# Concatenate vector
b <- c(1, 2, 3, 45, 10, 99, 6, 7, 8, 9)

# Data frames
df <- data.frame(a = a, b = b)

# Call a column of a dataframe using $
df$a 

# Matrix

matrix(NA, 3, 1)

matrix(NA, nrow = 1, ncol = 3) # the order matters !

matrix(NA, 1, 3) # the order matters !

matrix(NA, ncol = 3, nrow = 1) # specify the arguments


# List

chr <- c("My", "name", "is", "Moritz")

lst <- list(chr, df, a)

# Create a basic scatter plot

x <- seq(1, 10, 1)
y <- seq(1, 100, 10)

plot(x = x, y = y)

