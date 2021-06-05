"This file includes two functions, makeCacheMatrix() and cacheSolve(). 
makeCacheMatrix() creates an R object that stores a matrix and its inverse.  
cacheSolve() requires an argument that is returned by makeCacheMatrix() to 
retrieve the inverse from the cached value stored in the parent environment.
The aim of the project is to cache the inverse of a matrix in memory
rather than repeatedly calculating the same things so as to save time."

"1. makeCacheMatrix function creates a list containing four functions to 
set the value of the vector, get the value of the vector, set the value of 
the inverse, and get the value of the inverse"

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y)
    x<<-y
    i<<-NULL
get<-function()x
setinverse<-function(inverse) i<<-inverse
getinverse<-function()i
list(set=set,get=get,
     setinverse=setinverse, getinverse=getinverse)
}


"2. cacheSolve()function was designed to retrive the inverse from 
makeCacheMatrix()."

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
          message("getting the data")
          return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
