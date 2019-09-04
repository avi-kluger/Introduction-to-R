rm(list = ls())  # Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off() # Clean Plots
cat ("\014")   # Clean the R console

load("ListenReversed.RData")

################################################################################
# function to build scales
################################################################################
if (!require('psych')) install.packages('psych'); library('psych')

# scaleName <- "SWL"
# df = l_df

# I thank Nadav Kluger for help with streamlining the functions.
buildScale <- function(scaleNames, df = l_df) {
  # Use for loop to automate scale buildScale
  for (scaleName in scaleNames) {
    items    <- grep(scaleName, names(df))
    scale_df <- df[, items]
    print(describe(scale_df))
    boxplot(scale_df)
    # Map allows to map more than one vector to a function with multiple FUNs.
    # It prints the name of the item preceding the output of stem.
    Map(function(name, value){
      cat(name)
      stem(value)
      }, 
      names(scale_df), scale_df)
    # Print a correlation matrix among the items
    print(as.dist(round(cor(scale_df, use = "pairwise.complete.obs"), 2)))
    print(psych::alpha(scale_df))
    # Extract Cronbach's alpha and store in a vector
      # scale_name <- scaleName
      # scale_name <- unlist(strsplit(scale_name, split='_'))[1]
      x <- psych::alpha(scale_df)[["total"]]["raw_alpha"]
      names(x) <- scaleName
      # names(x) <- paste(scale_name, collapse = "_")
      x <- round(x, 2)
      if (!exists("reliabilityVector")) {
        reliabilityVector <- vector(mode = "numeric", length = 0)
      }
      reliabilityVector <- c(reliabilityVector, x)
    assign("reliabilityVector", reliabilityVector, envir = globalenv())
    df[, scaleName] <- rowMeans(scale_df, na.rm = TRUE)
  }

  return(df)
  
}

# Create a vector to be used in the function

scaleNames <- c("Empathy", "PAIR", "support", "SWL")

sink("itemAnalysis.txt")
pdf("itemAnalysis.pdf")
l_df <- buildScale(scaleNames)
dev.off()
sink()

# Stem on the constucted scales with scale name in the output
Map(function(name, value){
  cat(name)
  stem(value)
  }, 
  names(l_df[, c(scaleNames)]), l_df[, c(scaleNames)])

# Find the numbers of the colomuns that contain the scale scores
scaleColumns <- which(names(l_df) %in% scaleNames)

# Select demographic variables
demographics <- c(grep("Age", names(l_df)): grep("tenure", names(l_df)),
                  grep("genderChar", names(l_df)): 
                    grep("yearsCategoryCharacter", names(l_df)))

# keep only demographics and scales (drop all items) and assign to a new df
# l_scale_df     <- l_df[, c(demographics, scaleColumns)]
l_df     <- l_df[, c(demographics, scaleColumns)]



scaleColumns <- which(names(l_df) %in% scaleNames)
corTable          <- data.frame(Measure = names(l_df[, scaleColumns]), 
                           stringsAsFactors = FALSE)

corTable$Measure  <- paste(1:nrow(corTable), corTable$Measure, sep = ". ")
#corTable$N       <- sapply(l_df[, scaleColumns], function(x) sum(!is.na(x)))
corTable$Mean     <- sapply(l_df[, scaleColumns], mean, na.rm = TRUE)
corTable$SD       <- sapply(l_df[, scaleColumns], sd, na.rm = TRUE)

corMatrix         <- cor(l_df[, scaleColumns], use = "pairwise.complete.obs")
corMatrix         <- as.data.frame(corMatrix)
for (i in 1:length(corMatrix)) corMatrix[i, i] <- reliabilityVector[i]
if (!require('weights')) install.packages('weights'); library('weights')
corMatrix         <- apply(corMatrix, 2, rd, 2)
for (i in 1:ncol(corMatrix)) corMatrix[i, i] <- 
                                        paste0("(", corMatrix[i, i], ")")
corMatrix[upper.tri(corMatrix)] <- NA
corMatrix
corTable            <- cbind(corTable, corMatrix)
row.names(corTable) <- NULL
colnames(corTable)  <- c("Measure", "Mean",	"SD", 1:nrow(corTable)	)
corTable[, c("Mean", "SD")] <- round(corTable[, c("Mean", "SD")], 2)
corTable

if (!require('stargazer')) install.packages('stargazer'); library('stargazer')
stargazer(
  corTable,
  digits = 2,
  title = "Correlation Table",
  type = "html",
  out = "Table2.doc",
  float.env = "sidewaystable",
  summary=FALSE, rownames=FALSE, single.row = TRUE,
  style = "qje", notes.append = FALSE, notes.align = "l",
          notes = paste0("Note. N = ", nrow(l_df), 
          " for all scales, except for subjective well-being, where N = ", 
          sum(!is.na(l_df$SWL)), ".")
)

save.image("corTable.RData")

table(l_df$gender)
table(l_df$gender, l_df$supervisorGender)
for (i in scaleColumns) {
              cat(names(l_df[i])) 
              print(t.test(l_df[l_df$supervisorGender< 3, i] ~ 
              supervisorGender[l_df$supervisorGender< 3], data=l_df))
}
