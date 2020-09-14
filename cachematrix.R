# makeCacheMatrix contains the setters and getters of the matrix x and
# its inverse i, also allows us to use the $ form of the extract operator
# to access the functions set(), get(), setinverse(), and getinverse()
# by their names. When the function ends, it returns a fully formed object
# of type makeCacheMatrix().
# set() function clears any value of i that had been cached by a prior
# execution of cacheSolve(), also defines the setter for the matrix x.
# get() function defines the getter for the matrix x.
# setinverse() function defines the setter for the inverse i.
# getinverse() function defines the getter for the inverse i.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve() attempts to retrieve the inverse from the matrix passed in as
# the argument, if we have a valid cached inverse, cacheSolve() returns it to
# the parent environment without recalculation, if we don't have a valid cached
# inverse, cacheSolve() gets the matrix from the input object, calculates its
# inverse and then returns the value to the parent environment.
# cacheSolve() function requires an input argument of type makeCacheMatrix().
# cacheSolve() is the only place where the solve() function is executed,
# which is why makeCacheMatrix() is incomplete without cacheSolve().

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}