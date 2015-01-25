## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
## initialy setting the inverse of a matrix as null
  inv <- NULL
  
##set the value of the matrix

  set <- function(y) {
    ##caches the inputted matrix to see if the matrix has been changed    
    x <<- y
    ##setting the inverse to null
    inv <<- NULL
  }
  
  
  get <- function() {
    x
  }
  
  
  setinv <- function(i) {
    inv <<- i
  }
  
  
  getinv <- function() {
    inv
  }
  
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)    
  
}



## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {       
  inv <- x$getinv()
##checks for inverse if already cached,if cached then it returns the inverse matrix from cache 
  if(!is.null(inv)) {
    
    message("getting cached inverse")
    return(inv)
  }
  
  
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
