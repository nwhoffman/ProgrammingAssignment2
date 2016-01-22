## Coursera R Programming - Programming Assignment 2
## This assignment tests the understanding of lexical scoping in R
## (how variable names are assigned?).

## This function sets the value of a matrix, gets the matrix, 
## solves for the inverse, and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Gets the value of x
  get <- function() x
  
  # Sets the inverse of the matrix
  setinv <- function(solve) m <<- solve
  
  # Gets the inverse 
  getinv <- function() m

  # Returns a list of functions that 
  # set the matrix, get the matrix, 
  # set the inverse and get the inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
  
}


## This function checks to see if the inverse of a matrix has been cached
## and then retrieves the cached data. If the inverse hasn't calculated,
## the matrix is solved and the inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
   m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
