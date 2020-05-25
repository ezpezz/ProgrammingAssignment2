## Two functions that can cache the inverse of a matrix

makeCacheMatrix <- function( m = matrix() ) {     
  ## Creates a special matrix object that can cache its inverse
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Then return the matrix
    m
  }
  
  ## Setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Then getting the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Part 2: Computing the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## To return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Or return the inverse if it is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}