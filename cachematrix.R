## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  var_1 <- function(a) {
    x <<- a
    inv <<- NULL
  }
  var_2 <- function() x
  inverse_1 <- function(inverse) inv <<- inverse
  inverse_2 <- function() inv
  list(var_1 = var_1,
       var_2 = var_2,
       inverse_1 = inverse_1,
       inverse_2 = inverse_2)
}



## Write a short comment describing this function



cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$var_2()
  inv <- solve(mat, ...)
  x$inverse_1(inv)
  inv
}