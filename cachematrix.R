## The following two functions are designed to aid in the calculation of the inverse of a matrix.
## As the calculation can computationally costly, these functions will cache the inverse
## of a matrix and return that result when called after the initial inverse is calculated.

## makeCacheMatrix uses lexical scoping and the <<- operator to create a special matrix that contains
## the functions to set and get the matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix())
{
     m <- NULL
     set <- function(y)
     {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve is called to actually calculated the inverse of a matrix. The result is passed into
## the cached variable m, which is used to check if the function is called again on the same matrix
## so it can just return the solution already stored and saves the time to recalculate each time.

cacheSolve <- function(x, ...)
{
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m))
     {
          message("Getting cached data.")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
