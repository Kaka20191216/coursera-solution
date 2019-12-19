## The function caches the value of inversed matrix so it can be looked up when we 
## need to use it again.

## first creates a special "vector" containing 
## value of the matrix
## value of the inversematrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setmatrix <- function(matrix) i <<- matrix
  getmatrix <- function() i
  list(set= set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## following function calculates the inverse of the special "matrix"
## created with the above function. It first checks to see
##if the matrix has already been calculated. If so, it gets
## inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse matrix of the data and sets the value in the cache via the setmatrix function

cacheSolve <- function(x, ...) {
  i <- x$getmatrix()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setmatrix(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
