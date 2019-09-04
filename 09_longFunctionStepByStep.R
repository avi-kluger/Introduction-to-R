# I thank Nadav Kluger for help with streamlining the functions.
rm(list = ls())
if(is.null(dev.list()) == FALSE) dev.off()
cat ("\014")

load("ListenReversed.RData")

# function for boxplot
buildScale <- function(scaleName){
  items <- grep(scaleName, names(l_df))
  boxplot(l_df[, items])
}

buildScale("trust")
buildScale("PAIR")

# increase readability by creating *scale_df* -- subset df
buildScale <- function(scaleName){
  items    <- grep(scaleName, names(l_df))
  scale_df <- l_df[, items]
  boxplot(scale_df)
}
buildScale("Empathy")

# add to function stem-and-leaf display
buildScale <- function(scaleName){
              items    <- grep(scaleName, names(l_df))
              scale_df <- l_df[, items]
              boxplot(scale_df)
              sapply(scale_df, function(x){
                print(names(x))
                stem(x)})
}
buildScale("trust")

# Add describe function from *psych* package. Note describe is wrapped w/ print
buildScale <- function(scaleName){
  if (!require('psych')) install.packages('psych'); library('psych')
  items    <- grep(scaleName, names(l_df))
  scale_df <- l_df[, items]
  boxplot(scale_df)
  sapply(scale_df, stem)
  print(describe(scale_df))
}

buildScale("support")

# Add a second argument to function with default
buildScale <- function(scaleName = "PAIR", df = l_df){
  items    <- grep(scaleName, names(df))
  scale_df <- df[, items]
  boxplot(scale_df)
  apply(scale_df, 2, stem)
  print(describe(scale_df))
}
buildScale(scaleName = "trust", df = l_df)
buildScale()
buildScale("trust")

# add alpha

# note that alpha is called together with the package name
# This prevents clashes between alpha and the graphics of R
buildScale <- function(scaleName, df = l_df){
  items    <- grep(scaleName, names(df))
  scale_df <- df[, items]
  print(describe(scale_df))
  boxplot(scale_df)
  sapply(scale_df, stem)
  print(psych::alpha(scale_df))
}
buildScale("trust")

# add two methods for correlation matrix
buildScale <- function(scaleName, df = l_df){
  if (!require('apaTables')) install.packages('apaTables')
  library('apaTables')
  items <- grep(scaleName, names(df))
  scale_df <- df[, items]
  print(describe(scale_df))
  boxplot(scale_df)
  apply(scale_df, 2, stem)
  print(psych::alpha(scale_df))
  print(as.dist(round(cor(scale_df), 2)))
  apa.cor.table(scale_df)
}
buildScale("trust")
buildScale("support")

# add two methods for correlation matrix BUT control of NA in cor
buildScale <- function(scaleName, df = l_df){
  if (!require('apaTables')) install.packages('apaTables')
  library('apaTables')
  items <- grep(scaleName, names(df))
  scale_df <- df[, items]
  print(describe(scale_df))
  boxplot(scale_df)
  apply(scale_df, 2, stem)
  print(psych::alpha(scale_df))
  print(as.dist(round(cor(scale_df, use = "pairwise.complete.obs"), 2)))
  apa.cor.table(scale_df)
}
buildScale("support")

# function to build scales.  Note: to output an object from a function, use 
# *return*

buildScale <- function(scaleName, df = l_df){
  items <- grep(scaleName, names(df))
  scale_df <- df[, items]
  print(describe(scale_df))
  boxplot(scale_df)
  apply(scale_df, 2, stem)
  print(psych::alpha(scale_df))
  print(as.dist(round(cor(scale_df, use = "pairwise.complete.obs"), 2)))
  apa.cor.table(scale_df)
  df[, scaleName] <- rowMeans(scale_df, na.rm = TRUE)
  return(df)
}

# run the function and assign its output to l_df.  l_df now has a new column
# with the name of the input to the argument (trust)
l_df <- buildScale("trust")
names(l_df)


# function to build scales that uses a loop inside the function, to process
# a vector of scales.  It returns the same input data frame with the added
# scales.

buildScale <- function(scaleNames, df = l_df) {
  # Use for loop to automate scale buildScale
  for (scaleName in scaleNames) {
    items <- grep(scaleName, names(df))
    scale_df <- df[, items]
    print(describe(scale_df))
    boxplot(scale_df)
    apply(scale_df, 2, stem)
    print(as.dist(round(cor(scale_df, use = "pairwise.complete.obs"), 2)))
    apa.cor.table(scale_df)
    print(psych::alpha(scale_df))
    df[, scaleName] <- rowMeans(scale_df)
  }
  return(df)  # return sends the object back to the Global Environment
}

# Call the function that can receive a vector of scales,
# and place its output in a new df
out_df      <- buildScale(c("trust", "Empathy", "PAIR", "support"))

