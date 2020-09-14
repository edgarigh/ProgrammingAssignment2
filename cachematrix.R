# makeCacheMatrix() function returns a fully formed object of
# type makeCacheMatrix().
# set() function clears any value of i that had been cached by a prior
# execution of cacheSolve(), also defines the setter for the matrix x.
# get() function defines the getter for the matrix x.
# setinverse() function defines the setter for the inverse i.
# getinverse() function defines the getter for the inverse i.
# cacheSolve() is the only place where the solve() function is executed,
# which is why makeCacheMatrix() is incomplete without cacheSolve().

# makeCacheMatrix contains the setters and getters of the matrix x and
# its inverse i, also allows us to use the $ form of the extract operator
# to access the functions set(), get(), setinverse(), and getinverse()
# by their names.

makeCacheMatrix <- function(x = matrix()) {
  # x is initialized as a function argument.
  i <- NULL # i is set to NULL.
  set <- function(y) {
    x <<- y # y is assigned to x in parent environment.
    i <<- NULL # NULL is assigned to i in parent environment.
    # This line of code clears any value of i that had been
    # cached by a prior execution of cacheSolve().
  }
  get <- function() x # Defines the getter for the matrix x.
  setinverse <- function(solve) i <<- solve
  # Defines the setter for the inverse i.
  # After setinverse() completes, the code uses the <<- form of the assignment
  # operator to assign the input argument to the value of i in the parent
  # environment.
  getinverse <- function() i # Defines the getter for the inverse i.
  # The last section assigns each of these functions as an element
  # within a list(), and returns it to the parent environment.
  list(set = set, # gives the name 'set' to the set() function.
       get = get, # gives the name 'get' to the get() function.
       setinverse = setinverse,
       # gives the name 'setinverse' to the setinverse() function.
       getinverse = getinverse)
  # gives the name 'getinverse' to the getinverse() function.
}
# When the function ends, it returns a fully formed object
# of type makeCacheMatrix().

# cacheSolve() attempts to retrieve the inverse from the matrix passed in as
# the argument, if we have a valid cached inverse, cacheSolve() returns it to
# the parent environment without recalculation, if we don't have a valid cached
# inverse, cacheSolve() gets the matrix from the input object, calculates its
# inverse and then returns the value to the parent environment.
# cacheSolve() function requires an input argument of type makeCacheMatrix().
# cacheSolve() starts with a single argument x, and an ellipsis that allows
# the caller to pass additional arguments into the function.
cacheSolve <- function(x, ...) {
  # The function attempts to retrieve an inverse from the object passed
  # in as the argument.
  # First, it calls the getinverse() function on the input object.
  i <- x$getinverse()
  # Then it checks to see whether the result is NULL.
  # If the value here is not equal to NULL, we have a valid, cached inverse
  # and can return it to the parent environment.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # If the result of !is.null(i) is FALSE, cacheSolve() gets the matrix
  # from the input object, calculates a solve(), uses the setinverse() function
  # on the input object to set the inverse in the input object, and then
  # returns the value of the inverse to the parent environment by printing
  # the inverse object.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
# cacheSolve() is the only place where the solve() function is executed,
# which is why makeCacheMatrix() is incomplete without cacheSolve().