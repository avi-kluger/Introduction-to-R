# 1.5 Factors
# Factors are used to store categorical data.
eye.col.v <- c("brown", "green", "brown", "blue", "blue", "blue")         #vector
eye.col.f <- factor(c("brown", "green", "brown", "blue", "blue", "blue")) #factor
eye.col.v
eye.col.f

# R will identify the different levels of the factor - e.g. all distinct values. 
# The data is stored internally as integers - 
# each number corresponding to a factor level.
levels(eye.col.f) # The levels (distinct values) of the factor (categorical var)
as.numeric(eye.col.f)  # As numeric values: 1 is  blue, 2 is brown, 3 is green
as.numeric(eye.col.v)  # The character vector can not be coerced to numeric
as.character(eye.col.f)  
as.character(eye.col.v) 

# 1.6 Matrces & Arrays
# A matrix is a vector with dimensions:
m <- rep(1, 20)    # A vector of 20 elements, all 1
dim(m) <- c(5, 4)  # Dimensions set to 5 & 4, so m is now a 5x4 matrix

# Creating a matrix using matrix():
m <- matrix(data = 1, nrow = 5, ncol = 4)# same matrix as above, 5x4, full of 1s
m <- matrix(1, 5, 4)                     # same matrix as above
dim(m)                                   # What are the dimensions of m?

# Creating a matrix by combining vectors:
m <- cbind(1:5, 5:1, 5:9)  # Bind 3 vectors as columns, 5x3 matrix
m <- rbind(1:5, 5:1, 5:9)  # Bind 3 vectors as rows, 3x5 matrix

# Selecting matrix elements:
m <- matrix(1:10, 10, 10)
m[2, 3]     # Matrix m, row 2, column 3 - a single cell
m[2, ]      # The whole second row of m as a vector
m[, 2]      # The whole second column of m as a vector
m[1:2, 4:6] # submatrix: rows 1 and 2, columns 4, 5 and 6
m[-1, ]     # all rows *except* the first one

# Other operations with matrices:

# Are elements in row 1 equivalent to corresponding elements from column 1:
m[1, ] == m[, 1] 

# A logical matrix: TRUE for m elements >3, FALSE otherwise:
m > 3 

# Selects only TRUE elements - that is ones greater than 3:
m[m > 3]

t(m)          # Transpose m     
m <- t(m)     # Assign m the transposed m
m %*% t(m)    # %*% does matrix multiplication
m * m         # * does element-wise multiplication

# Arrays are used when we have more than 2 dimensions. 
# We can create them using the array() function:

a <- array(data = 1:18, dim = c(3, 3, 2)) # 3d with dimensions 3x3x2
a <- array(1:18, c(3, 3, 2))              # the same array

# 1.7 Lists
# Lists are collections of objects. A single list can contain all kinds of 
# elements - character strings, numeric vectors, matrices, other lists, and 
# so on. The elements of lists are often named for easier access.

v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)
v3 <- c(TRUE, TRUE, FALSE)  # Logical vector, same as c(T, T, F)

# A list with four components
l1 <- list(boo = v1, foo = v2, moo = v3, zoo = "Animals!")
l2 <- list(v1, v2, v3, "Animals!")

# Create an empty list:
l3 <- list()
l4 <- NULL

# Accessing list elements:
l1["boo"]   # Access boo with single brackets: this returns a list.
l1[["boo"]] # Access boo with double brackets: this returns the numeric vector
l1[[1]]     # Returns the first component of the list, equivalent to above.
l1$boo      # Named elements can be accessed with the $ operator, as with [[]]

# Adding more elements to a list:
l3[[1]] <- 11        # add an element to the empty list l3
l4[[3]] <- c(22, 23) # add a vector as element 3 in the empty list l4. 

# Since we added element 3 to the list l4above, elements 1 and 2 will be 
# generated and empty (NULL).

l1[[5]] <- "More elements!" # The list l1 had 4 elements, we're adding a 5th here.

l1[[8]] <- 1:11 
# We added an 8th element, but not 6th and 7th to the listl1 above. 
# Elements number 6 and 7 will be created empty (NULL).

l1$Something <- "A thing"  # Adds a ninth element - "A thing", named "Something"

# 1.8 Data Frames
# The data frame is a special kind of list used for storing dataset tables. 
# Think of rows as cases, columns as variables. 
# Each column is a vector or factor.

# Creating a dataframe:
dfr1 <- data.frame( ID = 1:4,
                    FirstName = c("John", "Jim", "Jane", "Jill"),
                    Female = c(F ,F ,T ,T), 
                    Age = c(22, 33, 44, 55) )
dfr1$FirstName   # Access the second column of dfr1. 

## Levels: Jane Jill Jim John
# Notice that R thinks that dfr1$FirstName is a categorical variable 
# and so it is treating it like a factor, not a character vector. 
# Let us get rid of the factor by telling R to treat FirstName as a vector:

dfr1$FirstName <- as.vector(dfr1$FirstName)
# Alternatively, you can tell R you do not like factors from the start 
# using stringsAsFactors=FALSE

dfr2 <- data.frame(FirstName = c("John", "Jim", "Jane", "Jill"),
             stringsAsFactors = F)
dfr2$FirstName   # Success: not a factor.

# Access elements of the data frame:
dfr1[1, ]   # First row, all columns
dfr1[, 1]   # First column, all rows
dfr1$Age    # Age column, all rows

# Rows 1 and 2, columns 3 and 4 - the gender and age of John & Jim
dfr1[1:2, 3:4] 
dfr1[c(1,3), ] # Rows 1 and 3, all columns

# Find the names of everyone over the age of 30 in the data:
dfr1[dfr1$Age > 30, 2]

# Find the average age of all females in the data:
mean(dfr1[dfr1$Female == TRUE, 4])
