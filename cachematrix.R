## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL    ##we start off the caching process by assigning a NULL value to what will become the inverse variable out the inverse matrix variable
  set <- function(y) {
    x <<- y    ## this part of the cahcing process sets the matrix we are looing to invert
    i <<- NULL ## this part sets the inverse to NULL within the context of the cachematrix
  }
  get <- function() x ##this part of the list merely returns what we set in the set function
  setinverse <- function(inverse) i <<- inverse ##this part of the list sets the inverse function through the use another function variable
  getinverse <- function() i ##this part of the list just returns the inverse set above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)##creates a list of all four parts we defined in the function above

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##pulls the inverse matrix set in the makeCacheMatrix function
  if(!is.null(i)) {
    message("getting cached data")## if the cached inverse matrix is not null, then we pull the matrix
    return(i)
  }
  data <- x$get() ##if the cahced inverse matrix is not NULL then we get the matrix from Makecachematrix
  i <- solve(data, ...) ##calculates the inverse
  x$setinverse(i)##sets the inverse
  i
}
