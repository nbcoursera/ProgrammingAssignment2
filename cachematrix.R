## Utility functions for creating a wrapper object to auto-cache matrix inversions

#### USAGE ####
##
## cm <- makeCacheMatric(myMatrix)   - Create caching wrapper to existing matrix
## cm$get()                          - Get the source matrix
## cm$set(newMatrix)                 - Set the internal matrix data.  Will
##                                     invalidate any cached inverse data.
## cm$getinverse()                   - Get the inverse of the matrix
##                                     repeated calls will return cached data

## NOTE: As designed, the API has a significant flaw in that multiple calls to
## cacheSolve() or obj$getinverse() with differing arguments will always return
## the same value for the inverse, as the code does not cache and then compare
## the argument list and values and invalidate the cache if they have changed.
## (This is a serious problem in the supplied reference code for makeVector)
## Given that this would result in very surprising output for the caller (and we
## are claiming to only support the inverse, and not arbitrary linear systems),
## I have eliminated the option to pass arguments to cacheSolve and $getinverse.

## Builds a wrapped object for the passed-in matrix that will lazily evaluate
## the inverse of the matrix and cache the resultant value
makeCacheMatrix <- function(x = matrix())
{
  # Internal object data
  mtrx.data <- x
  mtrx.inverse <- NULL
  
  ## Methods
  
  # Getter and setter for the internal matrix data
  set <- function (y) { mtrx.data <<- y; mtrx.inverse <<- NULL; }
  get <- function () { return(mtrx.data); }

  # The caching is done internally to the object, rather than allowing
  # an external caller to call "setinverse" with potentially incorrect
  # values.
  getinverse <- function () {
    if (!is.null(mtrx.inverse)) {
      message("getting cached data")
      return (mtrx.inverse)
    }
    mtrx.inverse <<- solve(mtrx.data)
    return (mtrx.inverse)
  }
  
  list(set = set, get = get, getinverse = getinverse)
}

## Implemented for compatibility with the assignment, but the
## caller can just call var$getinverse() on the list returned
## by makeCacheMatrix instead of using this free function
cacheSolve <- function (x)
{
  return(x$getinverse())
}

