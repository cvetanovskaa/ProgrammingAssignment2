## These two functions are a pair which allow us to compute inverses of 
## matrixs and store them in the cache

## This function creates a special "matrix" that can cache its inverse.
## We follow the example laid out in makeVector, and pass all of the getters
## and setters to list()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function(y) { x <<- y; inverse <<- NULL; }
  get <- function() x;
  setInverse <- function(inv) inverse <<- inv;
  getInverse <- function() inverse;
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse);
}


## This function calculates the inverse of the special matrix. If it has 
## already been calculated we grab it from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse();
  if(!is.null(inverse)) { message("getting cached matrix"); return(inverse); }
  data <- x$get();
  inverse <- solve(data, ...);
  x$setInverse(inverse);
  
  inverse
}
