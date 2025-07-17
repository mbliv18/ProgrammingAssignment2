%r

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL  # Cached inverse
  set <- function(y){
    x <<- y      # Set the matrix value
    z <<- NULL   # Reset cached inverse
  }
  get <- function() x  # Get the matrix value
  setInverse <- function(inverse) z <<- inverse  # Cache the inverse
  getInverse <- function() z  # Retrieve the cached inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  z <- x$getInverse()  # Check for cached inverse
  if(!is.null(z)){
    message("getting cached data")  # Use cached inverse if available
    return(z)
  }
  mat <- x$get()  # Get the matrix
  z <- solve(mat, ...)  # Compute the inverse
  x$setInverse(z)  # Cache the inverse
  z  # Return the inverse
}

# Example usage: display a matrix before and after applying the functions
mat1 <- matrix(c(2, 1, 1, 2), nrow = 2)
print("Original matrix:")
print(mat1)
cm1 <- makeCacheMatrix(mat1)
print("Inverse of the matrix:")
print(cacheSolve(cm1))
