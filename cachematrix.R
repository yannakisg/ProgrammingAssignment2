## makeCacheMatrix - Creates a matrix that caches its inverse
## cacheSolve - Returns the inverse of the matrix that is returned by the makeCacheMatrix function

## In order to run this program you do the following e.g.:
##    m <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2))
##    cacheSolve(m)
## If you run the cacheSolve(m) it will return the cached result

## Creates a matrix that can cache its inverse. The argument is a matrix and I assume that 
## it is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the "matrix" that is returned by the above function (makeCaheMatrix)
## If the inverse has already been calculated it returns the cached value, otherwise it finds
## the inverse matrix and saves it to the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
