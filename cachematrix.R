## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setmatinv <- function(inverse) invmat <<- inverse
  getmatinv <- function() invmat
  list(set = set,
       get = get,
       setmatinv = setmatinv,
       getmatinv = getmatinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         invmat <- x$getmatinv()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setmatinv(invmat)
  invmat
}
