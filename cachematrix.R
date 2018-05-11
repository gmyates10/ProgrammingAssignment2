## makeCacheMatrix creates a special "matrix" object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve calls functions stored in the special "matrix" returned by makeCacheMatrix (above). 
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache. 
##If the input is new, it calculates the inverse of the data and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
}