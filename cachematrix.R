

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inv as NULL
  i  <- NULL
  ## set the value of the Matrix
  set <- function(y) 
  {
    x <<- y
    i  <<- NULL
  }
  ## get the value of the Matrix
  get <- function() x 
  ## set the value of the invertible matrix
  setinverse <- function(inverse) i  <<- inverse
  ## get the value of the invertible matrix
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  ## If matrix inverse is calculated
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Retrieve a matrix using get method
  data <- x$get()
  ## Set value of inverse of matrix
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
