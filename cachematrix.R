## Functions below will cache the inverse of a matrix instead of computing
## repeatedly

## Function 1: Cache the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function 2: Return cached matrix if it exists, otherwise calculate the 
## inverse directly

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}

## Example using a 2x2 matrix

inputMatrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(inputMatrix))