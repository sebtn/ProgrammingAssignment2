#------ By Seb Bedout --------------#
# setwd("~/R prog.coursera")

# Teh idea of the code is to store a time consuming and computing expensive process inside a cache,
# so we can reuse it several times, i.e when we are looping.

# makeChacheMatrix is a function list, includes: set, get, getinverse and setinverse.
# The symbol "<<-" will assing a numeric value, represented by de 'greater than side...' outside the function,
# the symbol allow R: "to modify a variable modify a variable declared outside of the current function 
# in which the reference to the variable is made".
# It's easier to understand a code when the results of the functions inside are interpreted, so:

# Let's define a matrix with MakecacheMatrix function and let's name it z <- makeCacheMatrix(matrix(some_matrix))
# some commands and their results interpretations:
# z$get(); gives back the original matrix "z".
# z$getinverse; returns the inverse, but with no cache storage function to it.


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
       
# cacheSolve(z); returns the matrix "z" inverse and stores it in the cache for future references.
# Because: "matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it repeatedly".       
# cacheSolve(z); gives back the matrix "z" inverse in storage. You can see it in the screen "getting cache data"      

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
