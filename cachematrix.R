## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# Below cacheSolve function calculates the inverse of the matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
  data <- x$get()
  if(ncol(data) == nrow(data) && det(data) != 0)
  {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    m <- solve(data)
    x$setInv(m)
    m
  }
  else
  {
    stop("This matrix is not invertible, please provide a square matrix with non-zero determinant")
  }
}
