## function makeCacheMatrix cotains 4 functions.

## variable invM stores inverse of a matrix
## function set set new matrix and reset invM to NULL
## function get is used to return matrix
## function setInvert save inverse of a matrix to variable invM
## function getInvert return inverse of this matrix

makeCacheMatrix <- function(x = matrix(...),...) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInvert <- function(solve) invM <<- solve
  getInvert <- function() invM
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)
  
}


## variable x is makeCacheMartix object. I used function getInvert to get inverse of a matrix
## if invert matrix is already in cache, it print message "getting cahce data" and return
## invert matrix that stores in cache. If there is no inverse of a matrix. I use
## solve function to compute inverse then return

cacheSolve <- function(x, ...) {
  
  inv <- x$getInvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInvert(inv)
  
  inv
}

x<-makeCacheMatrix(matrix(c(1,2,3,0,1,4,5,6,0),nrow = 3,ncol = 3))


print(x$get())

y<-cacheSolve(x)

print(y)

print(round(x$get()%*%y))

