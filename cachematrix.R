## Put comments here that give an overall description of what your
## functions do
## The Function will take a matrix make it to be a special matrix and calculate
## the inverse of the matrix then store it to a cache which can be call if we 
## call the function again

## Write a short comment describing this function
## this function turn a matrix to special matrix so we can store it on cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## function to calculate the inverse of special matrix created with 
## makeCacheMatrix function if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i ## Return a matrix that is the inverse of 'x'
}