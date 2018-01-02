## Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly . 
##These pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set=set, get=get, setinverse=setinverse, getinverse= getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  data <- x$get
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
    
  
}
