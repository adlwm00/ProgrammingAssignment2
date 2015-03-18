##  This program creates a special object that stores
##  a matrix and caches its inverse.

## This function creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL

  ## Set the value of the matrix.
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## Get the vallue of the matrix.

  get <- function() x

  ## Set value of the inverse.
  setinverse <- function(solve) m <<- solve

  ## Get value of the inverse.
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function checks to see if the inverse of the
## matrix has already been calulated. If it has, it 
## retrieves it from the cache. Otherwise, it calclulates
## the inverse and stores it into the cache for future
## use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  ## If inverse already stored, then retreive it.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## If inverse not stored, then calculate it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)


  m


}
