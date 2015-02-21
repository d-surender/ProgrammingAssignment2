## solution to Assignment: Caching the inverse of a Matrix
## creates a wrapper object around a matrix that can optionally store
## its inverse. exposes setters and getters for both. invalidates
## cache on set.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

## First test:
## x=matrix(1:4,2,2)
## m$get()
##    [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cacheSolve(m)
##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> cacheSolve(m)
##getting cached data
##   [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


## Second test:
##> x=matrix(c(1,2,3, 0,1,4, 5,6,0),nrow=3,ncol=3,byrow=TRUE)
##> x
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> m = makeCacheMatrix(x)
##> m$get()
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
##> cacheSolve(m)
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
##> cacheSolve(m)
##getting cached data
##     [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1