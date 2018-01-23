## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix, get the value of the matrix
## set the inverse of the matrix, get the vinverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setInverse <- function(solve) m <<- solve
       getInverse <- function() m
       list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
