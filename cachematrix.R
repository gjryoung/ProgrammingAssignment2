## Put comments here that give an overall description of what your
## the first function creates a special matrix object that can cache its inverse.
## The input into this function is simply a variable of type matrix.
## in order to inverse the matrix, it must be a square matrix. e.g. 2x2
## makeCacheMatrix Function
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix())
  {
  m <- NULL
  set <- function(y)
    {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)
    m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##cacheSolve Function 
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated and the matrix has not changed,
##then the cachesolve should retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## how to test this function
## how to test this function
## x <- matrix(1:4, nrow = 2, ncol = 2)
## m <- makeCacheMatrix(x)
## m$get()
##      [,1]  [,2]
## [1,]    1    3
## [2,]    2    4
## s <-cacheSolve(m)
## print(s)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5