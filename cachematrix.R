##  Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  ## makeVector creates a special "list" of functions 
  ## that can set and get the value of the matrix, and set and get the value of the inverse
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  ##create list of functions
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## the function will calculate the inverse only if it wasn't cached before
  m <- x$getinv() ##return the inverse of matrix x 
  if(!is.null(m)) {
    message("getting cached data") 
    return(m) ## returned cached value
  }
  data <- x$get()
  m <- solve(data, ...) ##inverse matrix
  x$setinv(m)
  m ##return inverse matrix
}