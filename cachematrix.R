##
## This file contains functions to enable the caching of matrix inversions
## in order to speed execution time by eliminating unnecessary calls to solve()
##
## makeCacheMatrix - creates a "special" matrix object that has a cached copy
##    		     of its inverse stored with it
## cacheSolve	- solves (inverts) one of the "special" matrices created by
##		  makeCacheMatrix. If the matrix has a cached value of the
##		  inverse, that is returned instead of calling solve() each time
##
## ------------------------------------------------------------------------------
##
## makeCacheMatrix - creates a "special" matrix object that has a cached copy
##    		     of its inverse stored with it
## Usage:
##      m = makeCacheMatrix( matrix(..) )   # creates a new matrix obj
##      cacheSolve(m)                   # will call solve() on the matrix
##      cacheSolve(m)                   # matrix has not changed, cached inverse returned
##      m$set(new matrix)		# change the matrix
##      cacheSolve(m)			# matrix has changed; solve() will be called
##
makeCacheMatrix <- function(x = matrix()) {
  ##
  ## ----- initialize the cache to NULL
  ##
  cached_inverse <- NULL
  ##
  ## ----- "set" will store a new matrix, so the cache is invalid
  ##
  set<- function(newMatrix) {
    x <<- newMatrix
    cached_inverse <- NULL
  }
  ##
  ## ----- "get" returns the matrix
  ##
  get<- function() x
  ##
  ## ----- "setinverse" sets the cached matrix inversion
  ##
  setinverse<-function(inverse) cached_inverse <<- inverse
  ##
  ## ----- "getinverse" returns the cached matrix inversion
  ##
  getinverse<- function() cached_inverse
  ##
  ## ----- the "matrix" object is a list, with the functions defined
  ## ----- so they can be accessed by name, as in m$getinverse()
  ##
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
## cacheSolve	- solves (inverts) one of the "special" matrices created by
##		  makeCacheMatrix. If the matrix has a cached value of the
##		  inverse, that is returned instead of calling solve() each time
##
cacheSolve <- function(x, ...) {
  ##
  ## ----- if the matrix object has a cached copy of the inverse
  ## -----       return that immediately
  ##
  cached_inverse <- x$getinverse()
  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  ## ----- else (no cached copy)
  ## -----     invert the matrix
  ## -----     store it in the cached
  ## -----     return it
  ##
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}
