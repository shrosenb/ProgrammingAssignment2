####################################################################
# The makeCacheMatrix function, creates a special "matrix".
#   It is a list containing a functions to:
#   set the value of the matrix
#   get the value of the matrix
#   set the value of the matrix inverse
#   get the value of the matrix inverse
####################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

####################################################################
# The cacheSolve function calculates the inverse of the special "matrix" 
# created with the makeCacheMatrix function.
#   It first checks to see if the inverse has already been calculated.
#   If it has - It gets the inverse from the cache and skips the computation.
#   If not    - It calculates the inverse (solve) of the data and sets 
#               the value in the cache via the setinverse function.
####################################################################
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
