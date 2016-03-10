makeCacheMatrix <- function(x = matrix()){
  
  ##This function creates a special "matrix" object that can cache its inverse.
  
  inv <- NULL ##sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##gets the value of the matrix
  setinverse <- function(inverse) inv <<- inverse ##sets value of inverse matrix
  getinverse <- function() inv ##gets value of inverse matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x,...){
  
  ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
  
}
