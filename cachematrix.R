## This R script will have functions that will cache
## the inverse of a matrix along with the matrix itself
## When a call for inverse is made, it first check if the
## cache has data. If not, it will compute the inverse 
## and cache the result and retur the result
## If the result is already cached, it will return the 
## result from cache

## The following function caclulates the inverse of matrix
## and caches it. The assumption is that the incoming 
## matrix is always inversible
makeCacheMatrix <- function(x = matrix()) {
  #the vaiable to hold the inverse  
  inverse <- NULL
  
  #sets a new matrix, just a convenience function
  ##assigns the matrix to the new value and reset inverse
  set <- function(anotherMatrix) {
    x <<- anotherMatrix
    inverse <<- NULL
  }
  
  #gets the original matrix
  get <- function() x
  
  #sets a new inverse of the matrix
  #this enables lazy evaluation of inverse
  setInverse <- function(newInverse){
    inverse <<- newInverse
  }
  
  #gets the inverse
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)  
  
}


## This function will take a matrix and resolves the inverse
##if the inverse is available in cache, it will return from cache
##if not, it will compute inverse cache and return the result
cacheSolve <- function(x, ...) {
  #get the inverse
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Got data from cache")
    inverse
  }else{
    message("No data in cache")
    #get the matrix
    mat <- x$get()
    #compute inverse
    inverse <- solve(mat,...)
    #cache the result
    x$setInverse(inverse)
    #return the result
    inverse
  }
  
}
