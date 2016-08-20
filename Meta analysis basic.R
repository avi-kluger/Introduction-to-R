rm(list = ls()) # Clean the Global Environment.
cat ("\014")    # Clean the R console.
# dev.off()     # Clean graphs, only if present.

#------------------------  Meta Analysis examples by Avi Kluger -- Class 1 ----------------------------------------#

# install.packages("meta")            #Run once for each computer, or when you want to update the package.

library(meta)                         #load the package for use, each time you start R, or after detaching it. 

r       <- c(.3, .24, .2, .31)        #Create a vector with correlations.
N       <- c(145, 473, 97, 73)        #Create a vector with sample sizes.
Study   <- c("Gross & John (2003)", 
             "Haga, Kraft & Corby (2009)", 
             "Kashdan et al. (2006)", 
             "Schutte et al. (2009)") #Create a vector with names of studies.
Study   <- paste0 ("Study: ", Study)

metacor(r, N, Study)                  #Run function metacor from "meta" to meta analyze correlations. 