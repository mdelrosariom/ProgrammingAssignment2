## Caching intermediate objects in R can be an efficient way to avoid
##re-evaluating long-running computations. 
##This is a function that is able to cache potentially time-consuming computations by avoiding the recompute of 
##of calculations.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   ##Objects with value NULL can be changed by replacement. 
  ##set the value of the matrix
  set <- function(y) {
    x <<- y   ## The <<- operator asigns a value to an object in an environment that is different from the current environment. so
    ## y is asigned to be x in this environment 
    m <<- NULL  ## and the m is asigned a NULL value 
    ## get the value of the matrix
    get <- function() x
    ## set the value of solve
    setsolve <- function(solve) m <<- solve
    ##get the value of solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
  }
}

## The following function calculates the inverse (solve) of the special "matrix" created with the above function.
##It first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
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
