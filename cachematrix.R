## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix/cacheSolve are two functions that enable caching of the inverse
## of a matrix. Because inverting a matrix is computationally expensive, caching could save
## significant time in some cases


## Write a short comment describing this function
## makeCacheMatrix() takes a matrix as argument, and creates an enhanced object that provides
## functions to retrieve and modify the matrix, as well as to store and retrieve its inverse. 
## The functions in this object are used by cacheSolve() to store a newly computed
## inverse and to retrieve a cached inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y){
    x<<- y     # x is changed in the parent environment
    inv<<-NULL  # inv is changed in the parent environment
  }
  get<- function() x
  setInv<- function(inverse){
    inv<<-inverse # inv is changed in the parent environment
  }
  getInv<- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve() takes as argument an object created by makeCacheMatrix() function above.
## It checks if an inverse is cached, and if so, returns it. Otherwise it computes and
## caches the inverse, and returns the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  else{
    message("not cached; finding inverse and caching.")
    matrix<-x$get()
    inv<-solve(matrix,...)
    x$setInv(inv)
    inv
  }
}
