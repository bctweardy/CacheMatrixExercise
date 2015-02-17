## The function makeCacheMatrix is a list of 
## sub-functions that will cache the inverse
## of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL  
      }
      
      get <- function() x 
            setmat <- function(solve) m <<- solve     
            getmat <- function() m
            list(set = set, get = get,
                 setmat = setmat,
                 getmat = getmat)
}


## The function cacheSolve receives the cache for 
## the matrix returned by the makeCacheMatrix() 
## function above. If the cache does not exists, 
## it computes the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      z <- x$getmat()
      if(!is.null(m)) {
            message("getting cached data") 
            return(m)
  }
  
      matrix <- x$get()
      m <- solve(mat, ...) 
      x$setmat(m)
      m
}

#Test case
m <- matrix(data = c(5, 12, 3, 7, 5, 19, 31, 27, 5), nrow = 3, ncol = 3)
m2 <- makeCacheMatrix(m)
cacheSolve(m2)