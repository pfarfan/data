## These functions compute the inverse of a matrix stored in a cache memory.If the matrix has already been computed
## the function will use the stored matrix to compute the inverse


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL

  set <- function(y) { ## Sets the matrix x in the cache
    x <<- y
    m <<- NULL 
  }
  
  get <- function() x ## gets the matrix from the cache
  
  setSolve <- function(solve) m <<- solve # sets the inverse matrix in the cache
  
  getSolve <- function() m ##gets the inverse matrix from the cache
  
  list(set = set, get = get,
  
       setsolve = setSolve,
       
       getsolve = getSolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
  m <- x$getSolve() ##this function assigns the matrix to m
  
  if(!is.null(m)) {                 ## If matrix m is not in the cache the function goes and gets it
    message("getting cached matrix")
    return(m)
  }
  
  data <- x$get() # If the matrix is in the cache then it gets it from there
  
  m <- solve(data, ...) ## compute the inverse of the matrix stored in the cache
  
  x$setSolve(m) ## sets the inverse of the matrix
  
  m
  
  
}
