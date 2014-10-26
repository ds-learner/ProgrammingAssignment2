## These funtions allow the caching of a matrix's inverse
## to avoid unnecessary re-computation.


## The function makeCacheMatrix() is a constructor 
## whose output is a matrix m and a list of functions  
## that can be called from another function (cacheSolve())
## in order to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the Cache matrix with NA values
  dims <- dim(x)
  nr <- dims[1]
  nc <- dims[2]
  
  ncell <- nr * nc
  
  m <- matrix(data=NA, nrow= nr, ncol= nc)
  
  set <- function(y) {
    x <<- y
    m <<- matrix(data=NA, nrow= nr, ncol= nc)
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}



## The function cacheSolve takes as input
## a matrix outputted by the makeCacheMatrix function
## and outputs the matrix's inverse.
## This function either computes the inverse or recalls
## the cached inverse that was previously computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if (!anyNA(m)) {
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  m
  
}
