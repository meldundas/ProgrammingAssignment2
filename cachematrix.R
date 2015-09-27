## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  #list of functions in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 
   m <- x$getinverse()
  
   if(!is.null(m)) {
      message("getting cached data")
      return(m)   ## Return a cached matrix that is the inverse of 'x'
    }
  
   data <- x$get()

   m <- solve(data, ...)
  
   x$setinverse(m)
  
   m              ## Return a solved matrix that is the inverse of 'x'
    
        
}
