##Student Name: Chedister Lane
##This function creates a matrix object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse<- NULL
  set<- function(y){
    x<<- y
    matrixInverse<<- NULL
  }
  get<- function() x
  setInverse<- function(solve) matrixInverse<<- solve
  getInverse<- function() matrixInverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Computes the inverse of the matri returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matrixInverse<- x$getInverse()
  if (!is.null(matrixInverse)){
    message("Getting Cached Data")
    return(matrixInverse)
  }
  data<- x$get()
  matrixInverse<- solve(data)
  x$setInverse(matrixInverse)
  matrixInverse
}