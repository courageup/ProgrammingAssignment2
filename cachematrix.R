## The functions cach the inverse of a matrix rather than compute it repeatedly
## to reduce the cost of the computation.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m.inv<-NULL
  get<-function() x
  set.inv<- function(matrix.z) m.inv <<- matrix.z
  get.inv<- function() m.inv
  list(get=get, set.inv=set.inv, get.inv=get.inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m.inv<-x$get.inv()
  if(!is.null(m.inv)){
    message("getting cached data")
    return(m.inv)
  }
  data<-x$get()
  m.inv<-solve(data,...)
  x$set.inv(m.inv)
  m.inv
  }
