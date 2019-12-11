## The R function cache time-consuming matrix inversion computation
## and store contents in a special vector.When we need to reuse it
## we can simply look it up rather than recomputing.

## set the value of the matrix
## get the value of the matrix
## set the value of the inversematrix
## get the value of the inversematrix

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
