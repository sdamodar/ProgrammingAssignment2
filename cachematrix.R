## Two functions included here are:
##   makeCacheMatrix()
##   cacheSolve()
## makeCacheMatrix returns a list of 4 funtions that set and get
## a matrix as well as set and get inverse matrix.  Call this function
## first on a matrix object
##
## cacheSolve is called 2nd on the list object returned by the
## makeCacheMatrix for a matrix object.  cacheSolve uses the functions
## in the list object returned by makeCacheMatrix to set inverse
## (if it is not already cached) and display inverse matrix.

## makeCacheMatrix
## is a function containing the following fours functions that
## creates a special "matrix" and caches its inverse:
##  (1) set the value of matrix
##  (2) get th value of matrix
##  (3) set the value of inverse matrix
##  (4) get the value of the inverse matrix
##
## Usage Example 1
## m1 <- makeCahceMatrix(cbind(c(1,3),c(2,4)))
## m1$get() will display the set matrix
## 
## Usage Example 2
## m2 <- makeCacheMatrix()
## m2$set(cbind(c(1,3),c(2,4)))
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve function is always called on an object
## after makeCacheMatrix() is run to do the following:
##   (1) display inverse matrix if is already set in cache.
##   (2) if inverse matrix is not set then calculate and set it
##       using the functions of the object.
## Usage Example 1
##  Call function with object m1 created by makeCacheMatrix:
##    cacheSolve(m1)
##  This woud display the inverse of matrix and set the inverse
##  in cache so subsequent calls on m1 would display from cached
##  as long as the matrix is not changed.
## Usage Example 2:  Similar to above.  use m2 instead of m1.
##    cacheSolve(m2)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
