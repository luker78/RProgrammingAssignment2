## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of a matrix
  get <- function() x
  ## set the matrix
  setmatrix <- function(solve) m <<- solve
  ## get the matrix
  getmatrix <- function() m
  list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}

## cacheSolve - computes the inverse of a makeCacheMatrix matrix object
## if the cached matrix has not changed then cacheSolve will return the
## cached inverse matrix, otherwise recomputes the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
