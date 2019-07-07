## The function makeCachematrix creates an object with the matrix and stores the inverse of the matrix.

## The function cahceSolve returns the inverse of the cached matrix that has been stored in the 
## makeCachematrix function 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Stores the matrix in cache and sets the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
## Retrieves the matrix stored in the cache and returns the inverse of the matrix
