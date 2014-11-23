## Put comments here that give an overall description of what your
## functions do

## The functions follow the same basic format as the example in the
## assignment 2 instructions, first creates a function to produce a matrix
## that can cash its inverse, the second either gets the inverse of the matrix
## from the cash, or calculates it if it isnt there

## Write a short comment describing this function
## see above comments first part

makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## see above comments second part

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
