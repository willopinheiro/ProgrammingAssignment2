## These functions are used to cache and retrieve the inverse of a matrix.

## This function creates a especial matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y    
    inv <<- NULL  
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

## functioin that generates the inverse of the especial matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}