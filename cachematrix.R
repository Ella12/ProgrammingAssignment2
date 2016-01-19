
## The  following functions create an object  of matrix that can be stored in cache and have
## access functions to this object , like  in oop style .
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix . If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache, otherwise it would call the makeCacheMatrix function
## to compute the inverse for the first time


## This function receives a matrix which is inversible and creates an object with functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix". It recives the object retuned by
## makeCacheMatrix and returns the inverse of a matrix stored in this object or calculates it if it was not 
## cached before.
## Example : 
## #make inversible matrix as described in R help
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8); 
## x = makeCacheMatrix(h8)
## cacheSolve(x)

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <-try(solve(data))
  x$setinverse(i)
  i
    
}




