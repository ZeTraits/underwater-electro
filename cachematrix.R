## Together these two functions will cache the inverse of a created matrix

## This function caches a matrix so that the inverse may be found using cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function creates an inverse of the matrix found by calling makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'inv'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}