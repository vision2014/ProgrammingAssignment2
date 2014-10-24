## Put comments here that give an overall description of what your
## functions do
##--------------------------------------------------------------------
# Caching the Inverse of a Matrix

## Write a short comment describing this function
# this function creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(solve) m <<- solve
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# Conditions
# if the inverse exists and matrix has not changed the retrieve inverse else calculate inverse
# and cache the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
  
}

## test
# val <- makeCacheMatrix()
# val$set(rbind(c(1, -1/4), c(-1/4, 1)))
# cacheSolve(val)


