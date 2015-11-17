## Since (large) matrix inversion is a costly operation,  there are some benefit to caching the inverse of a matrix.
## The following two functions cache the inverse of a matrix, and call it if it has already been computed.

## The first function, makeCacheMatrix creates a special "matrix", which is a list containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## creates a special "matrix" as defined above
  m <- NULL
  set <- function(y) {
    x <<- y ## caching the matrix
    m <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) m <<- matinv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The second function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
