## A pair of functions that cache the inverse of a matrix.
## makeCacheMatrix returns a list of fucntions that set and retrieve the matrix and its inverse.
## cacheSolvecomputes the inverse of the matrix.If the inverse has already been calculated then the inverse from the cache will be retrieved.

## function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y #set x in the containing enviroment to be a new matrix
            m <<- NULL #set cached inverse matrix to NULL
      }
      get <- function() x #used to call the current matrix
      setinverse <- function(inverse) m <<- inverse #set the cached inverse matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
      m <- x$getinverse() #get current value for inverse to check if it has already been calculated
      if(!is.null(m)) {
            message("getting cached data")
            return(m) #if inverse already calcuated skip calculation
      }
      data <- x$get()
      m <- solve(data, ...) #calc inverse
      x$setinverse(m)
      m
}
