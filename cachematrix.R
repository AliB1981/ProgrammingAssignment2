## This function does two things. First it creates a vector which caches
## the inverse of a matrix, then it returns the cached inverse if it has
## not change. If it had changed, it calculates the inverse.

## Write a short comment describing this function
## This function creates a vector that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## This function computes the inverse of the above matrix. 
## If the matrix is unchanged the function returns cached inverse 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
