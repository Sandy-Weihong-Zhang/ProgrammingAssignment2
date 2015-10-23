## This is programming assignment#2 in the intro to R Programming.  
## The overall goal of these two functions is to make a matrix, 
## and compute its inverse, and to use its cache version.  Since 
## computation of the inverse of a huge matrix is time-consuming,
## we should use what's in the cache memory to avoid computing 
## its inverse again if it's already computed and stored. 
## This could be very helpful in practice if huge matrices are present.


## This function makeCacheMatrix takes in a matrix, and get its
## inverse.   

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  ## $get() just repeats what the input matrix is.
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() solve(x)
  ## this function returns the following list
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse by using its cache's version; 
## so it doesn't have to do the computation again if it's already 
## computed and stored in the cache memory.   

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## if inv is already there (i.e.,not null)
    ## then use its cache version, and exit (i.e. return) immediately.
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)          
    }
    ## otherwise, use solve() to get its inverse
    data <- x$get()
    inv <- solve(data, ...)  
    x$setinverse(x)
    inv
}
