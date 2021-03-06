
## The following functions represents an enhanced matrix and the function to process its inverse.

## This function defines the special "matrix" object
## that is capable of store the matrix and its own inverse matrix.

makeCacheMatrix <- function(activeMatrix = matrix()) {
## This is the internal object that holds the inverse. I need to be declared to R know it exists!.
  inverseMatrix <- NULL
## Set the matrix and reset the inverse to be calculated.
## we do not calculate the inverse, because we dont know if it will be required.
  set <- function(newMatrix) {
    activeMatrix <<- newMatrix
    inverseMatrix <<- NULL
  }
## Return the matrix
  get <- function() {
    activeMatrix
  }
## Store the inverse matrix
  setInverse <- function(inverse) 
    {
    inverseMatrix <<- inverse
  }
## Return the calculated inverse, or null if it need to be calculated.
## An enhancement will be calculate the inverse if it is null 
  getInverse <- function() {
    inverseMatrix
  }
## This declaration exposes the functions and the name they are going to be expoxed, in this case the same name
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of a matrix using the special "matrix" created
## and save it to the cache. If it already exists as catched it just return the cache.

cacheSolve <- function(x, ...) {
## First I get the current inverse matrix to check if it is already calculated.
  inverse <- x$getInverse()
## Inspect if the matrix is set
  if(!is.null(inverse)) {
    message("getting cached matrix")
## If the matrix is already calculated, there is no need to calculate it again.
    return(inverse)
  }
## If the inverse was not catched I need to get the matrix to calculate it
  activeMatrix <- x$get()
## calculate the inverse of the matrix
  inverse <- solve(activeMatrix, ...)
## Set the matrix Inverse Cache.
  x$setInverse(inverse)
## REturn the calculated inverse
  inverse
}

