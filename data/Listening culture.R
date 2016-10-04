#dev.off()
rm(list = ls()) #Clean the Global Environment
cat ("\014")    #Clean the R console

library(psych);library(GPArotation)

setwd("C:/Users/owner/Dropbox/Files/Courses/R/Disruptive Behavior Study")


header <- scan("Wave1.csv", nlines = 1, what = character())
header <- unlist(strsplit(header,","))

data1 <- read.csv("Wave1.csv", skip = 2, header = FALSE)
data2 <- read.csv("Wave2.csv", skip = 2, header = FALSE)
Nurse <- rbind(data1,data2)

colnames(Nurse) <-header
rm(list = c('data1','data2', 'header'))
names(Nurse)

for (i in 13:25) {
  Nurse [,i] <- ifelse (Nurse [,i] > 1 & Nurse [,i] < 10, Nurse [,i] +1, Nurse [,i] )
  Nurse [,i] <- ifelse (Nurse [,i] == 12, 2, Nurse [,i] )
  }


#################Factor Analysis for each sample separately #######################
fit <- principal(Nurse [13:25])
fit
fit$values;fit$values/17

fit <- fa(Nurse [13:25],nfactors = 2,rotate = "promax")
fit

alpha(Nurse[,c(13:16,19,23)]) #Constructive
alpha(Nurse[,c(17:18,22,24,25)]) #Destructive

Nurse$Constructive <- rowMeans(Nurse[,c(13:16,19,23)])
Nurse$Destructive <- rowMeans(Nurse[,c(17:18,22,24,25)]) 
x <- Nurse[,c(121:122)]
cor(x, use = "complete.obs")
