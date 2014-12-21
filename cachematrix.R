# Run the following commands to check if the functions are working.
#source: cachematrix.R
#jx <- makeCacheMatrix(x)
#jx$set(x)
#cacheSolve(jx)
# NOTE: x is the matrix that is to be inversed. 
# use cbind, rbind or matrix to create a square matrix that needs to be inverted.

## makeCacheMatrix creates a list of functions to operate on a matrix.

makeCacheMatrix <- function(xm = matrix()) {
  set <- function(ym) {
    xm <<- ym
    im <<- NULL
  }
  get <- function() xm
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
## returns the inverse if it is already calcuated and present in the environment.
## else runs the solve() command to get the inverse of the matrix passed as argument.
cacheSolve <- function(xm, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- xm$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- xm$get()
  
  #im <- solve(data)
  im <- tryCatch(solve(data), error = function(e) e)
  if (any(class(im) == "error")) {
    print("Use a invertible matrix");
    xm$setinverse(NULL)
  } else {
    xm$setinverse(im)
  }
  im
}
