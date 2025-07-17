## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when matrix is changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return a list of all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)  # Return cached inverse
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setinverse(inv)       # Cache the inverse
  inv
}

## Example test case 
# m <- matrix(c(1, 2, 3, 4), 2, 2)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)
# cacheSolve(cm)  # Should display "getting cached data"
