## Short comment describing this function
## makeCacheMatrix is a class basically, it establishes var x in the class as a matrix, and once the class action
## of set is called, it takes in that x matrix and sets it or caches/holds it
##
## getInverse and setInverse deal with feeding in the inverse solution and then returning it when it is requested

makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() { return(x) }
  setInverse  <- function(inverse) { inv  <<- inverse }
  getInverse  <- function() { return(inv) }
  list(set= set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

## cacheSolve is similar to the example cacheMean - it just checks if the inverse already exists in the makeCacheMat class
## then will return if it already exists, so long as trueTest is true (makes sure the mat is still the same)
## else it does the same as it would otherwise which is run solve() and get inverse, then setInverse in the matCache class

cacheSolve <- function(x, ...) {
  inv2  <- x$getinverse()
  if (!is.null(inv2)){
    trueTest <- all(x2 == x)
    if (trueTest == TRUE) {
      message("getting cached matrix data")
      return(inv2)
    }
    else {
      data  <- x$get()
      inv2  <- solve(data, ...)
      x$setInverse(inv2)
      return(inv2)      
    }
  }
  data  <- x$get()
  inv2  <- solve(data, ...)
  x$setInverse(inv2)
  return(inv2)
}
