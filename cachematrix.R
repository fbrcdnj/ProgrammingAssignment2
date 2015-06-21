## Similar to the makeVector function, the makeCacheMatrix creates a list containing functions to 
## set the value of the matrix, get the value of the matrix, set the value of the inverse matrix and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {				# a function to set the value of the matrix to given matrix and inverse matrix to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x				# a function to give the value of the matrix
  setinv <- function(z) inv <<- z		# a function to set the value of the inverse matrix from given inverse matrix
  getinv <- function() inv 			# a function to give the value of the inverse matrix
  list(set = set, get = get,			# create the list containing previous functions
       setinv = setinv,
       getinv = getinv)
}

## The following function computes the inverse of the matrix if it hasn't already been calculated;
## if it was already calculated, it will take it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()			# get the value on the inverse matrix from the list returned by makeCacheMatrix
  if(!is.null(inv)) {			# if inverse matrix is not null (i.e.: it has already been calculated)
    message("getting cached data")	# then 
    return(inv)				# get it from the cache and exit here
  }
  data <- x$get()			# or: get the matrix,
  inv <- solve(data, ...)		# invert it,
  x$setinv(inv)				# set it in the list, 
  inv					# and return the value.
}
