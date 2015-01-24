## Programming Assignement for Coursera - R Programming Course

## This program generates the inverse of a square matrix fed as an argument
## using the solve function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This program retuens the inverse of a square matrix. Retuens either a new 
## if not calculated already or from the cache if already computed

cacheSolve <- function(x, ...) {
  
  ## Uses the Solve function to return the inverse of the matrix
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}

