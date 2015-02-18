##Stephanie A Sanchez / 19FEB2015 / rprog-011 / Programming Assignment 2

## makeCacheMatrix and caseSolve are used to compute and cache the solution to the matrix inverse to avoid repeated computation.
## the input is assumed to be a square invertible matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## Use the "<<-" operator to "cause a search to made through parent environments for an existing definition of the variable being assigned. 
    ## If such a variable is found (and its binding is not locked) then its value is redefined, otherwise assignment takes place in the global environment"
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Retrieving cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

##Demonstrate implementation and result of these functions:
##> test<-matrix(c(0,1,2,0),2,2)
##> test
##[,1] [,2]
##[1,]    0    2
##[2,]    1    0
##> matrix<-makeCacheMatrix(test)
##> cacheSolve(matrix)
##[,1] [,2]
##[1,]  0.0    1
##[2,]  0.5    0
##> cacheSolve(matrix)
##Retrieving cached data...
##[,1] [,2]
##[1,]  0.0    1
##[2,]  0.5    0
