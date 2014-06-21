## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(computedInverse) inverse <<- computedInverse
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # prerequires x is a square invertible matrix

  # if the cache is empty (no inverse inside x)
  if(is.null(x$getinverse())) {
    # then compute the inverse from the matrix inside x
    print("cache empty => computing inverse")
    computedInverse <- solve(x$get())
    x$setinverse(computedInverse)
  } else print("inverse already cached")
  # at this point we are sure x contains a cached inverse,
  # either it already existed either it has just been computed
  x$getinverse()
}
