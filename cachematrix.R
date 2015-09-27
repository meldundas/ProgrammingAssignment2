## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL             #clear the contents of m
  
  set <- function(y) {  #changes matrix stored in main function
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  #returns matrix stored in main function
  
  #store the value of the inputin a variable m into the main
  #function makeCacheMatrix
  setinverse <- function(solve) m <<- solve  
            
  #retrieve the value from makeCacheMatrixe
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
 
  #check for previously calculated inverse matrix 
   m <- x$getinverse()

   #if there is:
   
   if(!is.null(m)) {
      message("getting cached data")
      return(m)   ## Return a cached matrix that is the inverse of 'x'
    }
  
   #if tehre is no cached data:
   
   #data gets the matrix stored with makeCacheMatrix
   data <- x$get()

   #calculate the inverse matrix and store in m
   m <- solve(data, ...)
  
   #store the calculated inverse matrix in the object instantiated 
   #with makeCacheMatrix
   x$setinverse(m)
  
   m              ## Return a solved matrix that is the inverse of 'x'
    
        
}
