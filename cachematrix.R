## Caches the results of solve() to avoid re-computing on the same matrix

## Store matrix and its inverse in parent environment
## Expose get and set methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(theInverse) inv <<- theInverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Generates a matrix inverse on a makeCacheMatrix
## Or returns cached inverse, if one already exists.

cacheSolve <- function(x, ...) {
  ## Attempt to retrieve existing inverse matrix
  inv <- x$getInv()
  
  ## Inverse is cached already
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Inverse has not been cached
  
  ## Retrieve matrix
  data <- x$get()
  
  ## Generate inverse
  inv <- solve(data, ...)
  
  ## Cache inverse matrix
  x$setInv(inv)
  
  ## Return inverse
  inv
}
