## makeCacheMatrix emulates a cache on top of a matrix
## cacheSolve is a able to use the emulated matrix from makeCacheMatrix

## Given a matrix this function stores its inverse
makeCacheMatrix <- function(x = matrix()) {
  # the cached inverse of the matrix
  inverse <- NULL
  # replaces the matrix with the newMatrix
  # and invalidates the cached inverse of the previous matrix
  set <- function(newMatrix) {
    x <<- newMatrix
    inverse <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse computed elsewhere
  setinverse <- function(computedInverse) inverse <<- computedInverse
  # get the cached inverse (can be NULL)
  getinverse <- function() inverse
  # functions accessible from the value returned by makeCacheMatrix
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Given an x this function is able
# - to retrieve a possibly cached inverse inside x
# - to compute the inverse of the contained matrix and store it inside x
# Returns the inverse of the matrix inside x
cacheSolve <- function(x, ...) {
  # prerequires x is a square invertible matrix

  # if the cache is empty (no inverse inside x)
  if(is.null(x$getinverse())) {
    # then compute the inverse from the matrix inside x
    print("cache empty => computing inverse")
    computedInverse <- solve(x$get())
    # set the computed inverse inside x (so that it is cached)
    x$setinverse(computedInverse)
  } else print("inverse already cached")
  # at this point we are sure x contains a cached inverse,
  # either it already existed either it has just been computed
  # it is safe to return the cached inverse from x
  x$getinverse()
}
