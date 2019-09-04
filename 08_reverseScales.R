rm(list = ls())                             # Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off() # Clean Plots
cat ("\014")                                # Clean the R console

load("ListenRecoded.RData")

# Reverse coding function

reverseCoding <- function(scale2reverse, scaleColumns2reverse) {
                columns                   <- grep(scale2reverse, names(l_df))
                columns2Rev               <- columns[scaleColumns2reverse]
                temp                      <- l_df[, columns2Rev, drop = FALSE]
                l_df[, columns2Rev]       <- 10 - temp
                names(l_df) [columns2Rev] <- paste0(names(temp), "Rev")
                # assign("l_df", l_df, envir = globalenv())
                # assign("test_df", temp, envir = globalenv())
                return(l_df)
}

head(l_df[1:3, grep("Empathy", names(l_df)) ])
l_df <- reverseCoding("Empathy", c(2, 4, 7, 10, 11, 13, 14))
head(l_df[1:3, grep("Empathy", names(l_df)) ])

l_df <- reverseCoding("PAIR", 6:15)
head(l_df[1:3, grep("support", names(l_df))])
l_df <- reverseCoding("support", 5)
head(l_df[1:3, grep("support", names(l_df))])
l_df <- reverseCoding("Communal_Strength", c(5, 7, 10))
l_df <- reverseCoding("perspective_taking", c(2, 4, 7, 8, 10, 12, 15, 18))
l_df <- reverseCoding("Rudeness", 1:3)
l_df <- reverseCoding("JDI", c(2, 3, 8, 9, 11, 12, 14, 16, 18))
l_df <- reverseCoding("Respect", c(2, 5, 6, 7, 8, 10, 13, 16, 18, 20))
l_df <- reverseCoding("PRI", 9:16)
l_df <- reverseCoding("Autonomy_Support", c(4, 5, 9:11, 13:15, 18:21))

# FLS -- Destructive listening scale:
# 11-20 (no need to reverse, just to create a sub scale)
FLS                     <- grep("FLS", colnames(l_df))
names(l_df)[FLS[1:10]]  <- paste0("constructive", names(l_df)[FLS[1:10]])
names(l_df)[FLS[11:20]] <- paste0("destructive",  names(l_df)[FLS[11:20]])



rm(list=setdiff(ls(), ls()[grep("l_df|Columns|Names", ls())]))
save.image("ListenReversed.RData")


pVector <- data.frame(Z.value     = numeric(), 
                      probability = numeric())

for (i in seq(0, 5, .1)) {
  p                  <- 1 - pnorm(i)
  nextRow            <- nrow(pVector) + 1
  pVector[nextRow, ] <- c(i, p)
}
plot(pVector, main = "p values for Z values between 0 and 5" )
