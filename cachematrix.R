##  Makes a special "matrix" object consisting of a list of functions allowing for cache and recall of the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  force(x)
  inv <- NULL
  set <- function(m){
    x<<-m
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(y) inv<<-y
  getinv <- function() inv
  list(set= set, get=get, setinv=setinv, getinv=getinv)
}


## Checks for a chached inverse and returns the cache value if it exsist. Solves for the inverse if not.

cacheSolve <- function(x, ...){
  inv<-x$getinv() 
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  m<-x$get()
  inv<-solve(m ,...)
  x$setinv(inv)
  return(inv)
}
