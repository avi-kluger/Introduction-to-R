rm(list = ls())
if(is.null(dev.list()) == FALSE) dev.off()
cat ("\014")

load("corTable.RData")

#ignore the "Other" category in the gender variables
l_df <- l_df[which(l_df$gender == 1 | l_df$gender == 2), ]
l_df <- l_df[which(l_df$supervisorGender == 1 | l_df$supervisorGender == 2), ]

browseURL("https://www.statmethods.net/stats/regression.html")

# regression with dummy code (same as ANOVA)
fit <- lm(support ~ gender, data = l_df)
fit
methods(class = class(fit))
summary(fit)
confint(fit)

# regression with a continous predictor
fit1 <- lm(support ~ PAIR, data = l_df)
summary(fit1)
confint(fit1)

# Inspect the scatterplot of this regression
browseURL("https://www.statmethods.net/graphs/scatterplot.html")

plot(l_df$support, l_df$PAIR, 
     main="Scatterplot of support by intimacy",
     xlab="Intimacy (PAIR) ", ylab="Support ", pch=19)

# Add the regression line to the scatterplot

abline(fit1, col="red")

# Test quadratic effect
fit2 <- lm(support ~ PAIR + I(PAIR^2), data = l_df)
summary(fit2)
confint(fit2)

plot(l_df$PAIR, predict(fit2))

# Create a function to repeat the above for other variables

test.Curve <- function (DV, IV, df){
  x <- df[, IV]
  y <- df[, DV]
  fit <- lm(y ~ x + I(x^2))
  print(summary(fit))
  print(confint(fit))
  plot(x, predict(fit))
}

test.Curve("support", "PAIR", l_df)
test.Curve("SWL", "Empathy", l_df)
test.Curve("Empathy", "gender", l_df)

l_df$SWL[7] <- NA
test.Curve("SWL", "Empathy", l_df)

# This failed the confint and the plot -- needed investigation
# The crook was NA.  Solved with adding na.action = "na.exclude" 

test.curve <- function (y, x, df){
  x <- df[, x]
  y <- df[, y]
  fit <- lm(y ~ x + I(x^2), data = df, na.action = "na.exclude")
  print(summary(fit))
  print(confint(fit))
  plot(x, predict(fit))
}

test.curve("SWL", "Empathy", l_df)
