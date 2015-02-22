## set of functions creating the inverse of a matrix and storing its cache
##
## makeCacheMatrix is a function which
## takes matrix as an argument, and
## creates a set of functions which
## set and get the value of this matrix, and
## set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) s <<- solve
  get_inverse <- function() s
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve is a function which
## takes created matrix as an argument, and
## returns its inverse,
## by first checking the cache and, if stored, returning cached value
## or creating a new inverse matrix through the solve() function

cacheSolve <- function(x, ...) {
  s <- x$get_inverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$set_inverse(s)
  s
}