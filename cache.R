##makeCacheMatrix takes a matrix and inverts it, then sets invert to the value of the invert of the matrix and caches it 

## this function creates a list of a matrix that solves its own inverse, allowing it to set the value of the vector and its inverse, and store them as cached data


makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invert <<- solve
  getinverse<-function(solve) invert
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 
##this function checks to see if matrix and inverse are stored as cached data, and if so, returns the cached data rather than recalculating

cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setinverse(invert)
  invert
}
