## The function Calculates the inverse of the matrix and caches the ouput

## makeCacheMatrix function  makes a cache matrix to calculate the inverse on the matrix

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


## cacheSolve calculates the inverse of the matrix if the data is not present as the cache otherwise it returns the cached data

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
