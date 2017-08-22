## These functions can cache the inverse of a matrix instead of computing it repeatedly

## This function can create a matrix object that will cache its inverse
## The function contains a vector that contains functions to do the following
## set the value of the matrix, get the value of the matrix, set value of inverse, get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function can compute inverse of the matrix from the above makeCacheMatrix.
## First, it checks if the inverse has been computed. If yes, it gets the inverse from cache and skip computation.
## If not, it computes the inverse of the mean and set the value in the cache through the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
