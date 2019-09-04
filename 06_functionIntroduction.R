rm(list = ls())
if(is.null(dev.list()) == FALSE) dev.off()
cat ("\014")
 
# 1. Simple example of the principle
# basic structure
f <- function(arguments) {
  ## Do something interesting
}

# most basic
f <- function(a) {
  a^3
}
f(a = 5)
f(1:5)

# basic
calcPercent <- function (fraction) {
  percent <- paste0(fraction * 100, "%")
  percent
}
calcPercent(5/7)

# add rounding and use *cat* to output to console
calcPercent <- function (fraction) {
  percent <- paste0(round(fraction * 100, 1), "%")
  cat(percent)
}
calcPercent(5/7)
calcPercent(50/7)
calcPercent(5/70)
calcPercent(5)

# the output from a function is only its last line 
# output two lines from a function with *print*
calcPercent <- function (fraction) {
  percent <- paste0(round(fraction * 100, 1), "%")
  print(c("The final calculation is ..."))
  cat(percent)
}
calcPercent(5/7)

# put two outputs in the last line
calcPercent <- function (fraction) {
  percent <- paste0(round(fraction * 100, 1), "%")
  cat("The final calculation is ...", percent)
}
calcPercent(5/7)

# Practice: Write a function that accepts a price and adds to it 17% VAT.
# name the function calculate.VAT.  To embelish, add to it NIS suffix.



# 2. A function with multiple arguments
Cohen.s.d <- function (m1, m2, s1, s2, n1, n2) {
  diff      <- m1 - m2
  product1  <- (n1 - 1) * s1^2
  product2  <- (n2 - 1) * s2^2
  numerator <- product1 + product2
  denom     <- n1 + n2 -2
  pooled    <- sqrt(numerator/denom)
  d         <- diff/pooled
  # print(paste0("Cohen's d = ", round(d, digits = 3)))
  round(d, digits = 3)
}
Cohen.s.d (1, .5, 1, 1, 10, 10)
Cohen.s.d (1, .5, 2, 1, 10, 10)

# Simple for loop.  Note. You must use print to output the results
for (i in 1:10){
  d <- Cohen.s.d (1, .5, 2, 1, 10, i)
  print(d)
}

# Use for loop and output its results to a data frame
# Create an empty dataframe.
dVector <- data.frame(sampleSize = numeric(), 
                         CohenSd = numeric())
dVector

for (i in 1:10){
  d <- Cohen.s.d (1, .5, 2, 1, 10, i)
  dVector[i, ] <- c(i, d)
}
dVector

# Empty the datafrme
dVector <- dVector[0, ]
dVector

# Perform a Monte Carlo study on the effect of n2 on d
for (i in seq(10, 1000, by = 10)) {
  d <- Cohen.s.d (1, .5, 2, 1, 10, i)
  dVector[i/10, ] <- c(i, d)
}

plot(dVector)
plot(dVector, main = "d value when means are 1 and 0.5, SDs 2 and 1, sample
     size in group 1 is 10, by sample size in group 2" )

# Practice: Create a for loop to calculate percents for dividing a numeric 
# vector by 7.  Make the loop run on all the numbers between 1 and 10
# If you don't like the appearance of your output, consult:
# https://stackoverflow.com/questions/4055762/in-r-can-i-stop-printcat-from-returning-null-and-why-does-catfoo-ret



