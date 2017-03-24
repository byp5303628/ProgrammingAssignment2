## This function contains four functions
## set to init and set inverse to NULL
## get to return init matrix
## setinverse to set the cached inverse matrix to variable `inverse`
## getinverse to get the cached inverse matrix, if it is not null, return directly.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will first get the cacheMatrix's inverse matrix
## if the inverse matrix has already been calculated, return it directly and log "getting cached data"
## else calculate the inverse matrix and cache it through setinverse function.
## then return the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
