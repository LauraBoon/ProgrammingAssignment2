## R Programming, Assignment 2 Due Sept 27th 2015
## Written by Laura Boon

## This function creates a list of 4 functions 
## 1) set: sets the value of the matrix as x and m 
##    (the inverse of the matrix) as null. Used when 
##    changing the base matrix.
## 2) get: The value of the matrix.
## 3) setinverse: sets the variable m as the value of 
##    the inverse of the matrix, x. 
## 4) getinverse: returns the value of the inverse, m 
##    when called. Or null if not yet calculated.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- x$setinverse(data)
  m
}
