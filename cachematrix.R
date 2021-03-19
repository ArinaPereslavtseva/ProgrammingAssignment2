makeCacheMatrix <- function(sam = matrix()) {
  invsam <- NULL
  set <- function(x) {
    sample <<- x
    invsam <<- NULL
  }
  get <- function() sam
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() invsamp
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
cacheSolve <- function(sam, ...) {
  inv <- sam$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invsamp)
  }
  mat <- sam$get()
  invsamp <- solve(mat, ...)
  sam$set_inverse(invsam)
  invsamp
}