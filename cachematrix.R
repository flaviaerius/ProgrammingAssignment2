## The functions hereby were created to store a variable of class matrix 
## and its inverse in the cache, to reduce computing expense to calculate it 
## repeatedly, when you need to retrieve it again during your work in R.

## First, you need to create the 'special vector' to be stored in the cache, with variables able
## to be stored in the cache (different environment).
## For this, you use the function makeCacheMatrix, written below:

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## To calculate the inverse of the matrix, you need to feed the below function 
## with the name of the 'special vector' created with the function makeCacheMatrix.
## There is no need to store it in a variable.

## The second time you call this function, it will be faster because you will be retrieving
## the result from the cached data, as pointed by the message 'getting cached data'

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

## matrix created to test the functions, using runif(), which creates uniform distributions:
# > matrixtest <- matrix(runif(25), nrow = 5, ncol = 5)
## create the 'special vector'
# > z <- makeCacheMatrix(matrixtest)
## compute the inverse
# > cacheSolve(z)
## try again, to see if you get the message
# > cacheSolve(z)

