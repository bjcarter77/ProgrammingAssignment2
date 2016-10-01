## This pair of functions perform Matrix inversion, caching the inverse 
## of a matrix rather than computing it repeatedly

## This 1st function takes the matrix x and caches it to the parent 
## environment, creates the vector m and sets it as NULL
## then it applies the solve function to find the inverse of x
## and caches into the parent environment in m

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This 2nd function first checks the parent environment to 
## check if inverse of x has been cached, if it has, the cached data
## is returned with the message, if not, the function then applies 
## the solve function to the matrix to return the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
