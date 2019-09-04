rm(list = ls())
if(is.null(dev.list()) == FALSE) dev.off()
cat ("\014")

load("corTable.RData")

#ignore the "Other" category in the gender variables
l_df <- l_df[which(l_df$gender == 1 | l_df$gender == 2), ]
l_df <- l_df[which(l_df$supervisorGender == 1 | l_df$supervisorGender == 2), ]

browseURL("https://www.statmethods.net/stats/anova.html")

# One-way ANOVA basic.  Note R tests Type I SS.  Order matters!

aov(support ~ genderChar, data = l_df)
fit <- aov(support ~ genderChar, data = l_df)
fit
# print the results of the assignment in one line with extra *()* 
(fit <- aov(support ~ genderChar, data = l_df))

methods(class = class(fit))
summary(fit)

fit <- aov(support ~ yearsCategoryCharacter, data = l_df)
summary(fit)

#Inspect means with apaTables
library('apaTables')
apa.1way.table(yearsCategoryCharacter, support, data = l_df)

# Tukey Honestly Significant Differences
TukeyHSD(fit) 

library('psych')
describeBy(l_df$support, l_df$yearsCategoryCharacter)

# Two-way ANOVA. 
fit2way <- aov(support ~ yearsCategoryCharacter*genderChar, data = l_df)
summary(fit2way)

# Order matters! R uses Type I SS.
fit2way <- aov(support ~ genderChar*yearsCategoryCharacter, data = l_df)
summary(fit2way)

apa.2way.table(genderChar, yearsCategoryCharacter, support, data = l_df)


apa.2way.table(genderChar, SgenderChar, support, data = l_df)
apa.2way.table(genderChar, SgenderChar, support, data = l_df,
               show.conf.interval = TRUE)
apa.2way.table(genderChar, SgenderChar, support, data = l_df,
               show.conf.interval = TRUE, filename = "firstANOVA.doc")
apa.2way.table(genderChar, SgenderChar, support, data = l_df,
               show.marginal.means = TRUE, filename = "secondANOVA.doc")

factor(df$yearsCategoryCharacter)
l_df$yearsCategoryCharacter <- ordered(l_df$yearsCategoryCharacter, levels = 
                c("Under a year", "1-2 years", "More than two years"))

if (!require('sciplot')) install.packages('sciplot'); library('sciplot')
bargraph.CI(yearsCategoryCharacter, support, group = SgenderChar, data = l_df,
            main = "Support by Gender and Years with Boss",
            xlab = "Years with boss",
            ylab = "Support",
            ylim = c(0, 10),
            cex.lab = 1.5,
            x.leg = 1,
            col = "black",
            angle = 45,
            cex.names = 1,
            density = c(0, 20),
            legend = TRUE)

