rm(list = ls())
cat ("\014")
if(is.null(dev.list()) == FALSE) dev.off()

load("Listen.RData")

# Create a new *df* with short name
l_df <- listenClean_df
rm(list=setdiff(ls(), c("l_df", "demographics")))

# ifelse Section ---------------------------------------------------------------

# minimal code demonstration
a <- 1:10
a
# Usage: ifelse(test, yes, no)
b <- ifelse(a > 8, 1, 0)
b
table(b)
c <- ifelse(a > 8, "top student", "other student")
c
table(c)

# the real thing
# Recode the gender variables. The survey had 1 - male; 2 - female; 3 - other

names(l_df[, demographics])

table(l_df$gender)
table(l_df$supervisorGender)

l_df$genderChar  <- ifelse(l_df$gender == 1, "Male", 
                           ifelse(l_df$gender == 2, "Female", NA))
l_df$SgenderChar <- ifelse(l_df$supervisorGender == 1, "Boss Male", 
                     ifelse(l_df$supervisorGender == 2, "Boss Female", NA))
table(l_df$SgenderChar)

stem(l_df$yearsWithBoss)

l_df$yearsCategory <- ifelse(l_df$yearsWithBoss < 1, 1,
                             ifelse(l_df$yearsWithBoss < 2, 2, 3))
table(l_df$yearsCategory)

l_df$yearsCategoryCharacter <- ifelse(l_df$yearsWithBoss < 1, "Under a year",
                                  ifelse(l_df$yearsWithBoss < 2, "1-2 years", 
                                         "More than two years"))
table(l_df$yearsCategoryCharacter)

# A note on *if* in R -- VERY IMPORTANT! if operates only on an atomic vector

rm(list=setdiff(ls(), c("l_df", "demographics")))
save.image("ListenRecoded.RData")
