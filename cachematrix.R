## Put comments here that give an overall description of what your
## functions do
## The first function creates a list of functions that will get and retrieve values
## in conjunction with the second function

## Write a short comment describing this function
## the first function takes a matrix and sets the inverse to a NULL value (initially)
## a set function (to reset the matrix, if you wish) will also set the inverse to a NULL value
## get returns the original matrix (used in function 2)
## setInverse and getInverse will be used in cacheSolve to get and set the inverse (inv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function is used with an argument created by the first function
## getInverse is called to see if the inverse (inv from makeCacheMatrix) has already been calculated
## if it's not null, we print that the inverse for this instance of the matrix has already been
## calculated and inv is returned
## if it is null, that means we never calculated the inverse (never called setInverse)
## so we assign the matrix part to the variable matrixdata
## we then set the value of inv to the inverse of the matrixdata matrix (solve function built in R)
## and inv is returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("inverse already calculated")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setInverse(inv)
  inv
}
