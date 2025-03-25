## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when matrix is updated
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if available
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the matrix inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}
