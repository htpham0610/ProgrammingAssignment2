#
## makeCacheMatrix creates a special matrix object and support operations. 
## CacheSolve performs inverse operations on the matrix.
##
## makeCacheMatrix() return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # matrix placeholder
  inv_x <- NULL

  # matrix setter
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  # matrix getter
  get <- function() x

  # inverse setter
  setinverse<- function(inverse) inv_x <<-inverse

  # uinverse getter
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}
##
## The cacheSolve() function returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it,
## otherwise, it computes, caches, and returns it.
##
cacheSolve <- function(x, ...) {
  
  inv_x <- x$getinverse()

  ## if exists, return the inverse matrix
  if (!is.null(inv_x)) {
    message("Getting cached inverse matrix...")
  } else {
   ## Setting and return the invserse matrix
   message("Setting cached inverse matrix...")
   inv_x <- solve(x$get())
   x$setinverse(inv_x)
  }
   return(inv_x)
  
}
## 
## SAMPLE OUTPUTS
## 
## > x <- matrix (rnorm(9), nrow=3)
## >  cx <- makeCacheMatrix(x)
## > cx$get()
##           [,1]        [,2]      [,3]
## [1,]  0.9906091  0.13375300 0.2019707
## [2,] -0.1783101 -1.11213516 0.3715050
## [3,] -0.9527509 -0.01863651 0.8338115
## >  cacheSolve(cx)
## Setting cached inverse matrix...
##           [,1]        [,2]       [,3]
## [1,] 0.7985767  0.10003066 -0.2380046
## [2,] 0.1781072 -0.88362533  0.3505574
## [3,] 0.9164709  0.09454967  0.9351923
## >  cacheSolve(cx)
## Getting cached inverse matrix...
##          [,1]        [,2]       [,3]
## [1,] 0.7985767  0.10003066 -0.2380046
## [2,] 0.1781072 -0.88362533  0.3505574
## [3,] 0.9164709  0.09454967  0.9351923
## >
## 

