#------ By Sebastian Bedout --------------#
# setwd("~/R prog.coursera")

# makeChacheMatrix is a function list, includes: set, get, getinverse and setinverse.
# Let's define a matrix with MakecacheMatrix function and let's name it "z":
# some commands and their results:
# z$get(); gives back the original matrix.
# cacheSolve(z); returns the matrix "z" inverse and stores it in the cache for future references.
# Because "matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it repeatedly".
# z$getinverse; returns the inverse, but with no cache storage function to it.
# cacheSolve(z); gives back the matrix z inverse in storage. You can see in the screen "getting cache data"

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse
  
}
