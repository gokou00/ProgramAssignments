## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function takes a square matrix and cache's it 
#The function tests the matrix to see if it's a vaild square matrix if not
#will display a message telling the user to submit a vaild matrix and return the 
#matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  f <-  class(try(solve(x),silent=T))=="matrix"
  
  if(f == FALSE){
    message("Please use a valid matrix")
    return(x)
  }
  
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mat_inverse) m <<- mat_inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  

}


## The cacheSolve function checks to see if the matrix inverse has already been 
#solved if so it returns the cached value. If not it will solve the matrix and cache the result


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  
}
