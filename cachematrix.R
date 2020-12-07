## This function uses lexical scoping to calculate matrix inverses with cache

## Create an object of type makeCacheMatrix with getters and setters for the
## input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculate the inverse of the matrix in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}