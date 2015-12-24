## makeCacheMatrix() and cacheSolve() are functions for cacheing the inverse of 
## square matrix so that it has to be computed only once. In subsequent calls of 
## cacheSolve(), the inverse is not computed. Instead, the cached value is returned. 

## makeCacheMatrix takes the original matrix as input and creates a "special" matrix
## which is really a list containing functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve() tries to get the inverse of the matrix from the cache. Only if it 
## does not exist in the cache, it computes the inverse and sets the value of the 
## inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

## Alexander Sakowitz