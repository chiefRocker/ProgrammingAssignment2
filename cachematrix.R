## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create, set and get matrix. set and get inverse
makeCacheMatrix <- function(a = matrix()) {

  inv = NULL
  set = function(b) {
    a <<- b
    inv <<- NULL
  }
  
  get = function() a
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function
##check if inverse has been calculated. if yes, get cached. if no, calculate

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  ##check if the inverse has been calculated yet
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  ##if not, get calculate
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
  
}
