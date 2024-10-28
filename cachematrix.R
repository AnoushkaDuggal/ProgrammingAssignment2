# The 'MASS' package is used for generalized matrix inversions, which work for both square and non-square matrices
library(MASS)

# Function to create a matrix object that can cache its inverse
createCacheMatrix <- function(matrix = matrix()) {
  inv_matrix <- NULL   # Initialize the inverse as NULL

  # Function to set a new matrix
  setMatrix <- function(new_matrix) {
    matrix <<- new_matrix
    inv_matrix <<- NULL  # Reset cached inverse when matrix changes
  }

  # Function to retrieve the current matrix
  getMatrix <- function() matrix

  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv_matrix <<- inverse

  # Function to retrieve the cached inverse, calculating if not available
  getInverse <- function() {
    if (is.null(inv_matrix)) {
      inv_matrix <<- ginv(matrix)  # Compute the generalized inverse
    }
    inv_matrix %*% matrix  # Return the calculated inverse
  }

  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

# Function to compute or retrieve the cached inverse of a matrix
computeCacheInverse <- function(matrix_obj, ...) {
  inv_matrix <- matrix_obj$getInverse()  # Attempt to retrieve cached inverse
  
  # Check if the inverse is already cached
  if (!is.null(inv_matrix)) {
    message("Using cached inverse data!")
    return(inv_matrix)  # Return the cached inverse
  }
  
  # If not cached, calculate the inverse
  matrix_data <- matrix_obj$getMatrix()
  inv_matrix <- solve(matrix_data, ...)  # Calculate the inverse using 'solve'
  matrix_obj$setInverse(inv_matrix)      # Cache the newly computed inverse
  inv_matrix   # Return the inverse
}
