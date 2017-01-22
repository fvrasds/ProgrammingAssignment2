## makeCacheMatrix creates a list that includes the input data matrix, and that can cache its inverse
## cacheSolve retrieves the matrix inverse if it has been cached, and computes the inverse if it has not been cached

## makeCacheMatrix creates a list of 4 objects 
## (1) get is a data matrix 
## (2) set is used to update the value of the data matrix; 
## Note: when set is invoked, it resets to null the cached inverse of the data matrix
## (3) getinv holds the cached value of the inverse of the data matrix, after the inverse has been calculated 
## (4) setinv is used to update the cached inverse of the data matrix

makeCacheMatrix <- function(x = matrix()) {
  cached.inv <- NULL
  set <- function(y) {
    x <<- y
    cached.inv <<- NULL
  }
  get <- function() x
  setinv <- function(calc.inv) cached.inv <<- calc.inv
  getinv <- function() cached.inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. 
## It first checks if the inverse has been previously calculated nad cached.
## It returns the cached inverse if that exists, else it calculates and caches the inversesourcesource

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  calc.inv <- x$getinv()
  if (!is.null(calc.inv)) {
    print("Cached inverse")
    return(calc.inv)
  }
  data <- x$get()
  calc.inv <- solve(data,...)
  x$setinv(calc.inv)
  calc.inv
}
