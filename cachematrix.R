## these functions create a special matrix in order to
## to check if the inverse has been calculated or to cache it
## potentially saving time that would be spent by repeated
## calculations

## this fuction creates the special matrix, it can set or return
## the value of a matrix or its inverse if calculated

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)


}


## this function checks to see if the inverse of x has been 
##calculated, if it has been calculated then it is returned
## if not already calculated then the function will calculate
## and return

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
