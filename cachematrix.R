## Put comments here that give an overall description of what your
## functions do

### Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix changes
  }
  
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter for the inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse, if available
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return the cached inverse
  }
  
  # If not cached, calculate the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the inverse
}

