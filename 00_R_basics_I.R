# Copied from http://kateto.net/networks-r-igraph
# 1.1 Assignment
# You can assign a value to an object using assign(), <-, or =.

x <- 3         # Assignment
x              # Evaluate the expression and print result

y <- 4         # Assignment
y + 5          # Evaluation, y remains 4

z <- x + 17*y  # Assignment
z              # Evaluation
rm(z)          # Remove z: deletes the object.

z              # Error!

# 1.2 Value comparisons
# We can use the standard operators <, >, <=, >=, ==(equality) 
# and != (inequality). Comparisons return Boolean values: TRUE or FALSE 
# (often abbreviated to just T and F).

2==2  # Equality

2!=2  # Inequality

x <= y # less than or equal: "<", ">", and ">=" also work

# 1.3 Special constants
# Special constants include:
#   
# NA   for missing or undefined data
# NULL for empty object (e.g. null/empty lists)
# Inf  and -Inf for positive and negative infinity
# NaN  for results that cannot be reasonably defined

# NA - missing or undefined data
5 + NA      # When used in an expression, the result is generally NA
is.na(5+NA) # Check if missing

# NULL - an empty object, e.g. a null/empty list
10 + NULL     # use returns an empty object (length zero)
is.null(NULL) # check if NULL

# Inf and -Inf represent positive and negative infinity. They can be returned
# by mathematical operations like division of a number by zero:
5/0
is.finite(5/0) # Check if a number is finite (it is not).

# NaN (Not a Number) - the result of an operation that cannot be reasonably 
# defined, such as dividing zero by zero.
0/0
is.nan(0/0)

# 1.4 Vectors
# Vectors can be constructed by combining their elements with the 
# important R function c().
v1 <- c(1, 5, 11, 33)       # Numeric vector, length 4
v2 <- c("hello","world")    # Character vector, length 2 (a vector of strings)
v3 <- c(TRUE, TRUE, FALSE)  # Logical vector, same as c(T, T, F)

# Combining different types of elements in one vector will coerce the elements 
# to the least restrictive type:
v4 <- c(v1,v2,v3,"boo")     # All elements turn into strings

# Other ways to create vectors include:
v <- 1:7               # same as c(1,2,3,4,5,6,7)  
v <- rep(0, 77)        # repeat zero 77 times: v is a vector of 77 zeroes
v <- rep(1:3, times=2) # Repeat 1,2,3 twice  
v <- rep(1:10, each=2) # Repeat each element twice  
v <- seq(10,20,2)      # sequence: numbers between 10 and 20, in jumps of 2  

v1 <- 1:5         # 1,2,3,4,5
v2 <- rep(1,5)    # 1,1,1,1,1 

# Check the length of a vector:
length(v1)
length(v2)

# Element-wise operations:
v1 + v2      # Element-wise addition
v1 + 1       # Add 1 to each element
v1 * 2       # Multiply each element by 2
v1 + c(1,7)  # This doesn't work: (1,7) is a vector of different length

# Mathematical operations:

sum(v1)      # The sum of all elements
mean(v1)     # The average of all elements
sd(v1)       # The standard deviation
cor(v1,v1*5) # Correlation between v1 and v1*5 

# Logical operations:

v1 > 2       # Each element is compared to 2, returns logical vector
v1==v2       # Are corresponding elements equivalent, returns logical vector.
v1!=v2       # Are corresponding elements *not* equivalent? Same as !(v1==v2)
(v1>2) | (v2>0)   # | is the boolean OR, returns a vector.
(v1>2) & (v2>0)   # & is the boolean AND, returns a vector.

# Vector elements:
v1[3]             # third element of v1
v1[2:4]           # elements 2, 3, 4 of v1
v1[c(1, 3)]       # elements 1 and 3 - note that your indexes are a vector

v1[c(T, T, F, F, F)]  # elements 1 and 2 - only the ones that are TRUE
v1[v1 > 3]        # v1>3 is a logical vector TRUE for elements >3

# Note that the indexing in R starts from 1, a fact known to confuse and upset people used to languages that index from 0.

# To add more elements to a vector, simply assign them values.
v1[6:10] <- 6:10

# We can also directly assign the vector a length:
length(v1) <- 15 # the last 5 elements are added as missing data: NA

############################ Practice ##########################################

# 1. Create a vector with the names of 4 children that you like.  Assign this
# vector to a new object named myChildren. Print this object to the console.

# 2. Create a vector with the ages of the 4 children that you like.  Assign this
# vector to a new object named childAge.  Print this object to the console.

# 3. Calculate the ages in childAge in months.  Please the results in a new 
# vector named childAgeInMonths.

# 4. Print to the console the ages in month of the 2nd and 3rd child

# 5. Print to the console the ages of children that are older than 60 months.

# 6. Place a vector created in #5 in a new vector named schoolAgeAges. Print it.



