# Function creates a special "matrix" object 
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("cached value of inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

# simple test
test <- function () {
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  cacheMatrix <- makeCacheMatrix(hilbert(3))
  print(cacheSolve(cacheMatrix))
  print(cacheSolve(cacheMatrix)) 
}


