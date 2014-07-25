######################################################################################
#This function uses the function provided in Assignment 2 of the Coursera Class
# Programming R as a general template.
# makeCacheMatrix returns a list of functions that relate to the matrix that is passed
# and stored by the state information of the makCacheMatrix.
# cacheSolve calculates the Inverse Matrix of the passed Specialmatrix returned by makeCacheMatrix.
# cacheSolve only solves the Inverse Matrix once and stores that information in a cache.  Future requests
# to return the Inverse Matrix of the passed special Matrix returns the Inverse Matrix from memory (cache)
# thereby avoiding the CPU processing time for calcualing the Inverse Matrix more than once.
# If the Matrix itself is changed, the Inverse Matrix is set to NULL because the Inverse Matrix cached 
# value (in memory) will no long be valid.  This enables cacheSolve to update the cache to contain
# the correct Inverse Matrix.

makeCacheMatrix <- function(x = matrix()) { 
  InverseMatrix <- NULL # Initalize the Inverse Matrix to Null
  setMatrix <- function(newMatrix) { # Receives a Matrix Object
    x <<- netMatrix    # Sets the function state object, a matrix x, to a new matrix
    InverseMatrix <<- NULL # Sets the Inverse matrix to null because the Matrix has changed and thus the inverse matrix has changed
  }
  getMatrix <- function() x #returns the matrix
  setInverse <- function(solve) InverseMatrix <<- solve # sets the inverse matrix to the higher scope
  getInverse <- function() InverseMatrix # returns the inverse matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,  #here is the special list
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) { #cacheSolve either solves the inverse Matrix of the passed matrix x or returns a cached solution of the inverse Matrix of x
  MatrixInverseTry <- x$getInverse() #lookup variable that may include the cached inverse matrix
  if(!is.null(MatrixInverseTry)) { # This if statement returns the Cached Inverse Matrix if it exists.  That is, if the inverse matrix has already been solved 
    message("getting cached data")
    return(MatrixInverseTry)
  }
  data <- x$getMatrix()              #data holds a temporary copy of the Matrix x, which is a special Matrix that was passed to this function
  InverseMatrix <- solve(data, ...) #Calculates the inverse Matrix here.
  x$setInverse(InverseMatrix) #stores the Inverse Matrix so that it is cached in memeory
  InverseMatrix #returns the Inverse Matrix to fullfill the function, cacheSolve()'s, purpose
}
