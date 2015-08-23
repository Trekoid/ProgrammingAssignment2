## The following functions create a caching matrix object that can cache the
## inverse matrix to eliminate redundent caclculations in larger R programs.
## This is an application of Dynamic Programming techniques.
##
## Example usage:
## > a <- makeCacheMatrix(matrix(c(7,3,-2,5), 2, 2))
## > cacheSolve(a)
## > a$get() %*% cacheSolve(a)

## The makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  set <- function(y) {
    x <<- y
    imatrix <<- NULL
  }
  get <- function()
    x
  setInverse <- function(inverse)
    imatrix <<- inverse
  getInverse <- function()
    imatrix
  list(
    set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## The cacheSolve function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverted matrix has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value of the inverse matrix
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  imatrix <- x$getInverse()
  if (!is.null(imatrix)) {
    message("getting cached inverse matrix")
    return(imatrix)
  }
  data <- x$get()
  imatrix <- solve(data, ...)
  x$setInverse(imatrix)
  imatrix
}
