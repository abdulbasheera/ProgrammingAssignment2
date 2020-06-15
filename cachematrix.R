## makeCacheMatrix function stores the value of an inverse in a cache so that it can be extracted from their. 
## cacheSolve calculates the inverse and if its stored in the cache already, it extracts it.

## Stores the inverse in a cache

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<-solve
  getinverse <- function() s
  list(set = set, get=get,
       setinverse= setinverse,
       getinverse = getinverse)
}


## Calculates inverse or extracts it if its already calculated

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if (!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
        ## Return a matrix that is the inverse of 'x'
}


