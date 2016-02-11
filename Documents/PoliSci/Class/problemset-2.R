vector_data <- vector('numeric')
# Creates empty data set 
choose_values <- function(vector_data, returnvec = TRUE, returnmatrix = TRUE){
  if (returnvec == TRUE & returnmatrix == FALSE) {
    return(c(vector_data))
  }
  if (returnvec == FALSE & returnmatrix == TRUE) {
    return(matrix(c(vector_data), nrow = 1, ncol = length(vector_data)))
  }
  if ((returnvec == FALSE & returnmatrix == FALSE) | 
      (returnvec = TRUE & returnmatrix = TRUE)) {
    return()
  }
}
# Creates a function that takes in a data set and creates a vector or matrix by your choice.
vector_data <- c(1.3, 2.222, 0.44, 3.5, 4.6, 5.7, 6.99, 1.8, 10.6)
# Using a random group of 9 data values here
set <- choose_values(vector_data, TRUE, FALSE)
# Apply function to data values
set <- 10*set
# Multiply set of data by 10^x with x being the maximum number of 0's to the left of the first significant digit of any given value.
sig_one <- function(n) {
  as.numeric(head(strsplit(as.character(n), '')[[1]], n = 1))
}
# Takes the 1st digit of each value by using a function.
list_sigs <- sapply(set, sig_one)
# Creates a list of the 1st significant digit of every data value.
chart_one <- hist(list_sigs, 1:9)
# Creates a histogram, with 9 bars to account for every possible significant digit.
list_distrib <- chart_one$density
# Creating a vector with the proportion of the first significant digit being each number from 1 to 9.
m <- ((list_distrib - (log10(1 + (1 / 1:9)))))
# Using the formula, this is the m statistic for every number 1 through 9, not just the max.
d <- (list_distrib - (log10(1 + (1 / (1:9)))))^2
# Using the formula, this is the d statistic for each number 1 through 9 individually.
maxi <- max((list_distrib - (log10(1 + (1 / 1:9)))))
# This is the m statistic
dstat <- sqrt(sum((list_distrib - (log10(1 + (1 / (1:9)))))^2))
# This is the d statistic
all_function <- function(list_distrib, returnm=TRUE, returnd=TRUE) {
  if (returnm == TRUE & returnd == TRUE) 
    return(list("m"= maxi,"d"= dstat, "fullm" = m, "fulld" = d))
  if (returnm == TRUE & returnd == FALSE) return(list("m"=m, "fullm" = m))
  if (returnm == FALSE & returnd == TRUE) return(list("d"=d, "fulld" = d))
  if (returnm == FALSE & returnd == FALSE) return()
}
print(all_function(list_distrib, TRUE, TRUE))
# This function returns a list of the m statistic and d statistic for each 
# significant digit and then returns its maximum and square root of the two respectively.
sigm <- ifelse (maxi<0.851, print("NF"), ifelse (maxi<0.967, print("*"), 
        ifelse (maxi<1.212, print("**"), print("***"))))
sigd <- ifelse (dstat<1.212, print("NF"), ifelse (dstat<1.33, print("F1"), 
        ifelse (dstat<1.569, print("F2"), print("F3"))))
# Calculates using a boolean whether each statistic reaches certain critical values.
print.benfords <- function(maxi, dstat, sigm, sigd) {
  sample_matrix <- matrix(data = c(maxi, dstat), nrow = 1, ncol = 2)
  colnames(sample_matrix) <- c("Leemis'm", "Cho-Gains'd")
  sample_matrix <- rbind(sample_matrix, c(sigm, sigd))
  legend_part1 <- c((" NF = No Fraud "), 
  (" ** = Fraud with critical value of 0.05 ")) 
  legend_part2 <- c((" * = Fraud with critical value of 0.1 "), 
  (" *** = Fraud with critical value of 0.01 "))
  legend_matrix <- matrix(data = c(legend_part1, legend_part2), nrow = 2, ncol = 2)
  sample_matrix <- rbind(sample_matrix, legend_matrix)
  return(as.table(sample_matrix))
}
# Creates a table with all four elements:  names of the statistics, the m and d stats,
# whether it passes the critical level tests, and the legend
write.table(print.benfords(maxi, dstat, sigm, sigd), file = "benford.csv")
# Creates an excel file with the table


