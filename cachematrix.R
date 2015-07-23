
## The following functions represents an enhanced matrix and the function to process its inverse.

## This function defines the special "matrix" object
## that is capable of store the matrix and its own inverse matrix.

makeCacheMatrix <- function(activeMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(newMatrix) {
    activeMatrix <<- newMatrix
    inverseMatrix <<- NULL
  }
  get <- function() {
    activeMatrix
  }
  setInverse <- function(inverse) 
    {
    inverseMatrix <<- inverse
  }
  getInverse <- function() {
    inverseMatrix
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of a matrix using the special "matrix" created
## and save it to the cache. If it already exists as catched it just return the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  activeMatrix <- x$get()
  inverse <- solve(activeMatrix, ...)
  x$setInverse(inverse)
  inverse
}

