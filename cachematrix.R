## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix:This function creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse<-NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse<-function(Inverse) matrixInverse<<-Inverse
  getInverse<-function() matrixInverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve would retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data)
  x$setInverse(Inverse)
  Inverse
}
