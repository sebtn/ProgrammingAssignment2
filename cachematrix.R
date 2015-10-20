#------ By Sebastian Bedout --------------#
# setwd("~/R prog.coursera")

# makevector almacena una lista de funciones. set, get, setmean, getmean
# el makevector siempre se almacena un vector llamada x, 
# usando $get(), se obtiene dicjho vector
# usando set, se cambia el vector almcaenado en la funcion ppal, se unsa cunado se quierecambiar el vector


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