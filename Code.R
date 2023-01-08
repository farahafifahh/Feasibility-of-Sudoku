# Sudoku 6x6 Rules
# The numbers of 1 to 6 must occur only once in each column.
# The numbers of 1 to 6 must occur only once in each row.
# The numbers of 1 to 6 must occur only once in each block of 2x3

# Set environment of file location
setwd("/Users/farahafifah/R Programming/Sudoku")

# Import data using scan() function
sudoku <- scan('sudoku.txt', sep=",")
sudoku <- matrix(sudoku, ncol = 6, byrow = 6)

# Check if the rows has duplicated numbers
inRow <- function(x){
  for(i in 1:6) {
    row <- x[i,]
    if(any(duplicated(row))){
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check if the columns has duplicated numbers
inCol <- function(x){
  for(i in 1:6) {
    col <- x[, i]
    if(any(duplicated(col))){
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check if 2x3 boxes has any duplicated numbers
inBox <- function(x){
  num <- c(1,3,5)
  for(i in num) {
    for(j in 3:4) {
      box_x <- ((i - 1) %/% 2) + 1
      box_y <- ((j - 1) %/% 3) + 1
      box_matrix <- x[(2 * box_x - 1):(2 * box_x),(3 * box_y - 2):(3 * box_y)]
      box <- as.numeric(box_matrix)
      if(any(duplicated(box))) {
        return(TRUE)
      } 
    }
  }
  return(FALSE)
}

# To test if the sudoku is feasibility or not
feasible <- function(x){
  if(isTRUE(inRow(x))) {
    ("Sudoku infeasible because there are some identical numbers in the same row.")
  }
  else if(isTRUE(inCol(x))) {
    ("Sudoku infeasible because there are some identical numbers in the same column.")
  }
  else if(isTRUE(inBox(x))) {
    ("Sudoku infeasible because there are some identical numbers in the 2x3 box")
  }
  else("Sudoku is feasible.")
}

# Function call sudoku 2x3
feasible(sudoku)
