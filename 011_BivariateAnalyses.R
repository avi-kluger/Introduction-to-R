rm(list = ls())  # Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off() # Clean Plots
cat ("\014")   # Clean the R console

load("corTable.RData")
#ignore the "Other" category in the gender variables
l_df <- l_df[which(l_df$gender == 1 | l_df$gender == 2), ]
l_df <- l_df[which(l_df$supervisorGender == 1 | l_df$supervisorGender == 2), ]

# ==X==============================================================X==
#          Crosstabulate frequency and use chi-Squre test.           =
# ==X==============================================================X==

#Crosstabulate frequency and use chi-Squre test.

table(l_df$gender, l_df$supervisorGender)
table(l_df$genderChar, l_df$SgenderChar)

browseURL("https://www.statmethods.net/stats/frequencies.html")

# 2-Way Cross Tabulation
if (!require('gmodels')) install.packages("gmodels"); library(gmodels)
CrossTable(l_df$genderChar, l_df$SgenderChar, chisq = TRUE)

if (!require('vcd')) install.packages("vcd"); library(vcd)
assocstats(table(l_df$genderChar, l_df$SgenderChar))

prop.test(x = c(189,  213), n = c(189+259,  213+ 104 ))

x <- table(l_df$genderChar, l_df$SgenderChar) 
y <- assocstats(x) # From vcd
########### Extract results  ###################################
x
y <- assocstats(x)
y
str(y)
y[["chisq_tests"]]["Pearson", "X^2"]
Chi <- y[["chisq_tests"]]
Chi
browseURL("http://web.pdx.edu/~newsomj/cdaclass/ho_phi.pdf")

#Extract the results to a table

bivariateTable <- data.frame(Technique = character(),
                             `Raw Effect` = numeric(),
                             `Standardized effect name` = character(),
                             `Standardized effect` = numeric(),
                             `Statistics name` = character(),
                             `Statistics` = numeric(),
                              stringsAsFactors = FALSE)
bivariateTable[1, c(1, 3, 5)] <- c("Contingency table", "Phi", "Chi-Square")
str(assocstats(x))
bivariateTable[1, c(2, 4, 6)] <- c(NA, assocstats(x)$phi, y[["chisq_tests"]]["Pearson", "X^2"])
                               
bivariateTable
# ==X==============================================================X==
#                     Two proportions z-test                         =
# ==X==============================================================X==
x <- table(l_df$genderChar, l_df$SgenderChar) 
x <- as.matrix(x)
n1 <- sum(x["Female", ])
n2 <- sum(x["Male", ])
p1 <- x["Female", "Boss Male"] / n1
p2 <- x["Male", "Boss Male"] / n2
p <- sum(x[, "Boss Male"]) / (n1 + n2)

numerator <- p2 - p1
SE <- p*(1-p) * ((1/n1) + (1/n2))
  z <- numerator / sqrt(SE)
z; z^2
z^2 == y[["chisq_tests"]]["Pearson", "X^2"]
round(z^2, 6) == round(y[["chisq_tests"]]["Pearson", "X^2"], 6)

bivariateTable[2, c(1, 3, 5)] <- c("Two proportions Z test", "", "Z-test")
str(assocstats(x))
bivariateTable[2, c(2, 4, 6)] <- c(p2-p1, NA, z)
                               
bivariateTable
# ==X==============================================================X==
#              t-test for indepenet means diffference                =
# ==X==============================================================X==
# To facilitate interpretation create dummy scores
table(l_df$supervisorGender)
l_df$supervisorGender <- l_df$supervisorGender - 1
table(l_df$supervisorGender)
l_df$gender <- l_df$gender - 1

t.test(supervisorGender ~ gender, data = l_df)

#reverse dummy codes to male = 1
l_df$gender           <- 1- l_df$gender 
l_df$supervisorGender <- 1- l_df$supervisorGender

t.test(supervisorGender ~ gender, data = l_df)
(fit <- t.test(supervisorGender ~ gender, data = l_df, var.equal = TRUE))
fit$statistic^2
Chi

if (!require('compute.es')) install.packages("compute.es"); library(compute.es)
ES <- tes(fit$statistic, n1, n2)

bivariateTable[3, c(1, 3, 5)] <- c("t-test for indepedent means", "Cohen's d", 
                                   "t-test")
str(fit)
str(tes(fit$statistic, n1, n2))
bivariateTable[3, c(2, 4, 6)] <- c(fit$estimate[2] - fit$estimate[1], 
                                   tes(fit$statistic, n1, n2)$d, fit$statistic)
                               
bivariateTable
# ==X==============================================================X==
#              correlation                                           =
# ==X==============================================================X==
cor(l_df$gender, l_df$supervisorGender)
cor.test(l_df$gender, l_df$supervisorGender)


bivariateTable[4, c(1, 3, 5)] <- c("Pearson's r", "r", 
                                   "t-test")
str(cor.test(l_df$gender, l_df$supervisorGender))
bivariateTable[4, c(2, 4, 6)] <- c(NA, cor(l_df$gender, l_df$supervisorGender),
                                   cor.test(l_df$gender,
                                            l_df$supervisorGender)$statistic)
bivariateTable
# ==X==============================================================X==
#              regression                                            =
# ==X==============================================================X==
lm(supervisorGender ~ gender, data = l_df)
p2-p1
fit <- lm(supervisorGender ~ gender, data = l_df)
summary(fit)
Chi


bivariateTable[5, c(1, 3, 5)] <- c("Regression", "beta", "F-test")
fit <- lm(supervisorGender ~ gender, data = l_df)
str(fit)
fitS <- lm(scale(supervisorGender) ~ scale(gender), data = l_df)
str(summary(fit))
bivariateTable[5, c(2, 4, 6)] <- c(fit$coefficients[2], 
                                   fitS$coefficients[2], 
                                   summary(fit)$fstatistic["value"])

bivariateTable[6, c(1, 3, 5)] <- c("Regression", "beta", "t-test")
bivariateTable[6, c(2, 4, 6)] <- c(fit$coefficients[2], 
                                   fitS$coefficients[2], 
                          summary(fit)[["coefficients"]][, "t value"][2])

bivariateTable
# ==X==============================================================X==
#              log-linear regression                                 =
# ==X==============================================================X==
x <- table(l_df$genderChar, l_df$SgenderChar) 

#Odds for having a males supervisor

OddMale     <- x["Male", "Boss Male"]   / x["Male", "Boss Female"]
OddFemale   <- x["Female", "Boss Male"] / x["Female", "Boss Female"]
pMale       <-  x["Male", "Boss Male"]   / 
               (x["Male", "Boss Male"] + x["Male", "Boss Female"])
pFemale     <- x["Female", "Boss Male"]   / 
              (x["Female", "Boss Male"] + x["Female", "Boss Female"])

Odd.Ratio.M <- OddMale   / OddFemale  
Odd.Ratio.F <- OddFemale / OddMale 

OddMale              ; OddFemale
Odd.Ratio.M          ; Odd.Ratio.F; 
pMale                ; pFemale
log(Odd.Ratio.M)     ; log(Odd.Ratio.F)
exp(log(Odd.Ratio.M)); exp(log(Odd.Ratio.F))

#print log of OR from vcd
oddsratio(x) 
exp(as.numeric(oddsratio(x) [[1]][1]))

fit <- glm(supervisorGender ~ gender, data = l_df, family = binomial())
summary(fit)                    # display results
confint(fit)                    # 95% CI for the coefficients

exp(coef(fit))                  # exponentiated coefficients
exp(confint(fit))               # 95% CI for exponentiated coefficients

#Predicted proportion of MALES who work for a male boss 
1 / (1+exp(- (coef(fit)[1] + coef(fit)[2]*1))) 
#Predicted proportion of FEMALES who work for a male boss (note the 0) 
1 / (1+exp(- (coef(fit)[1] + coef(fit)[2]*0))) 

bivariateTable[7, c(1, 3, 5)] <- c("Log-linear regression", "Odd-ratio", 
                                   "Z-test")
summary(fit)$coefficients
bivariateTable[7, c(2, 4, 6)] <- c(NA, exp(as.numeric(oddsratio(x) [[1]][1])), 
                                   summary(fit)$coefficients["gender", 3]) 

bivariateTable
ES$d
ES$r
ES$OR
1/ES$OR
res(cor(l_df$gender, l_df$supervisorGender), n = nrow(l_df))

CrossTable( l_df$yearsCategoryCharacter, l_df$genderChar, chisq = TRUE)
