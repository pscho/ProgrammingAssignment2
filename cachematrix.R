## These two functions create a special object that stores a matrix and
## calculates and caches the inverse of the matrix.

## makeCacheMatrix - Creates a "matrix" object that will cache the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve - Returns a matrix that is the inverse of the argument x
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(x, ...)
  x$setinverse(i)
  i
}
