## makeCacheMatrix uses four utiity functions akin to those in the example provided (see readme.R), 
## which write and read the matrix (get and set resp.), and write and read the inverse 
## (setinv and getinv resp.)
## These four functions are returned as a list

## cacheSolve takes a matrix as produced by makeCacheMatrix, and reads if there is an inverse 
## stored in the cache: if so, then it returns this (and a message saying that it has done so);
## otherwise it calculates the inverse of the matrix, stores it to the cache and returns this inverse


makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(x) {
    mat <<- x
    inv <<- NULL
  }
  get <- function() {
      mat
  }
  setinv <- function(inverse) {
      inv <<- inverse
  }
  getinv <- function() {
      inv
  }
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}
