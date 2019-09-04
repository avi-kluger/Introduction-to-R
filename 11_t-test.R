rm(list = ls())
if(is.null(dev.list()) == FALSE) dev.off()
cat ("\014")

load("corTable.RData")

#ignore the "Other" category in the gender variables
l_df <- l_df[which(l_df$gender == 1 | l_df$gender == 2), ]
l_df <- l_df[which(l_df$supervisorGender == 1 | l_df$supervisorGender == 2), ]

browseURL("https://www.statmethods.net/stats/ttest.html")

# t-test basic.  Note R does not assume equal variance
t.test(support ~ genderChar, data = l_df)

# plot a side-by-side boxplot
boxplot(l_df$support ~ l_df$genderChar)

# Embelish boxplot
boxplot(support ~ genderChar, data = l_df, 
        main="Feeling supported by one's manager", 
        xlab="Gender", 
        ylab="Support",
        notch = TRUE)

# getting means and SDs with apaTables
if (!require('apaTables')) install.packages('apaTables'); library('apaTables')

# Note that apaTable expect the IV first
apa.d.table(genderChar, support, data = l_df)
apa.d.table(genderChar, SWL, data = l_df)

# with psych
if (!require('psych')) install.packages('psych'); library('psych')
describeBy(l_df[, "support"], l_df$genderChar)

# Descriptive statistics for all variables in df by gender
describeBy(l_df, l_df$genderChar)
describeBy(l_df[, scaleColumns], l_df$genderChar)
describeBy(l_df[, scaleColumns], l_df$genderChar)
myList <- describeBy(l_df[, scaleColumns], l_df$genderChar)
lapply(myList, `[`,  c("n", "mean", "sd"))

       