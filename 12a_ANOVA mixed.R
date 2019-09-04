rm(list = ls())
cat ("\014")
if(is.null(dev.list()) == FALSE) dev.off()

load("corTable.RData")

#ignore the "Other" category in the gender variables
l_df <- l_df[which(l_df$gender == 1 | l_df$gender == 2), ]
l_df <- l_df[which(l_df$supervisorGender == 1 | l_df$supervisorGender == 2), ]

browseURL("http://www.cookbook-r.com/Statistical_analysis/ANOVA/")

repeatedDemom_df <- l_df[, c("genderChar", "SWL", "Empathy",
                                   "PAIR", "support")]
repeatedDemom_df$PID <- 1:nrow(repeatedDemom_df)

# Load the *tidyverse* packages
if (!require('tidyverse')) install.packages('tidyverse')
suppressMessages(library('tidyverse'))

# Reshape the data from wide to long
long_df <- gather(repeatedDemom_df, construct, attitude, -genderChar, -PID) 
# Exolore the long dataframe
head(long_df)
tail(long_df)

# Mixed ANOVA.  Note that PID must be a factor.
fit <- aov(attitude ~ genderChar*construct + Error(factor(PID)/construct), 
           data = na.omit(long_df))
summary(fit)
with(long_df, interaction.plot(construct, genderChar, attitude))
