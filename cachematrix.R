##Theis R function will cache matrix inverse calculation and read it, in order to same time
## save time to re-calcluate it repeatedly.

## makeCacheMatrix function is used to make a object that stores a matrix vector

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(abc) inv <<- abc
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve funcuton is use to check if the matrix has been cached
## it will get cached data if it has been calcuated.

cacheSolve<- function(x, ...){
  myinv <- x$getinverse()
  if(!is.null(myinv)) {
    message("getting cached data")
    return(myinv)
  }
  data <- x$get()
  myinv <- solve(data, ...)
  x$setinverse(myinv)
  myinv
}