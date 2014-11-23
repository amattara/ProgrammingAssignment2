## In this assignment I will develop some functions that cache the inverve ofa matrix


## The first function makes an object that can cache the inverse of the matrix
makeCacheMatrix <- function( mat = matrix() ) {
  
  ## First there's the initialization of the inverse
  n <- NULL
  
  ## Now I just set the matrix
  set <- function( matrix ) {
  mat <<- matrix
  n <<- NULL
  }
  
  ## This is the method of matrix returning
  get <- function() {
  mat
  }
  
  ##This is the method to set the inverse of the matrix
  setInverse <- function(inverse) {
  n <<- inverse
  }
  
  ## And now the method to get and return the inverse
  getInverse <- function() {
  n
  }
  
  ## Here the list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Now few words before proceeding with the second function:
## The inverse matrix computation is done by the previous function BUT
## if the inverse has already been calculated then  the following function
## have to retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## This return the inverse matrix of x 
  mat <- x$getInverse()
  
  ## This applies if the inverse matrix is already calculated
  if( !is.null(mat) ) {
    message("Processing cached data")
    return(mat)
  }
  
  ## Now  the matrix is retrieved from our object
  data <- x$get()
  
  ## Now the inverse is computed using matrix multiplication
  mat <- solve(data) %*% data
  
  ## Finally the matrix's inverse is set to the object
  x$setInverse(mat)
  
  ## In the last line the matrix is returned 
  mat
}
