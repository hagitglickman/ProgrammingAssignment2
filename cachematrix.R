
## This function defines 4 functions:'set' and 'get' for the value of a matrix x  
## and 'setinv'  and 'getinv' for the value of its inverse m. 
## The  function should be run only once with the input matrix and from that 
## point, the 'getinv' function will return the calculated inverse matrix from  
## the cache memory.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function derives the inverse matrix while using the cache memory from 
## the second time and onward. The function receives as input the list with the 
## four functions defined by makeCacheMatrix function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data: ",m)
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  message("getting calculated data: ",m)
}
