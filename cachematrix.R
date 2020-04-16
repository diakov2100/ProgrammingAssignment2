## Put comments here that give an overall description of what your
## functions do

## The best function on the planet that creates an object that stores matrix and caches its inverse variant 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       getsolve = getsolve,
       setsolve = setsolve)
}


## The second best function on the planet that thinks twice (once) before calculating matrix inverse variant
## from scratch as it may already store result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        
}
