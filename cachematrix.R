## Function to create a special matrix
makeCacheMatrix <- function(x = matrix()) {
  #x=matrix(1:4,nrow=2,ncol=2)
  inv_matrix <- NULL
  
  ## Get and Set methods for the original and inverse matrix
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) inv_matrix <<- solve(x)
  get_inv <- function() inv_matrix
  
  list(set=set, get=get, set_inv = set_inv, get_inv = get_inv)
  
}

## This function retrives the inverse of the matrix from the cache.
## If it is not in cache, it re-computes the inverse and returns the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##orig_matrix <- makeCacheMatrix()
  
  inv_matrix <- x$get_inv()
  if(!is.null(inv_matrix)) {
    message("Getting cached inverse")
    return(inv_matrix)
  }
  matrix <- x$get()
  inv_matrix <- solve(matrix)
  x$set_inv(inv_matrix)
  inv_matrix
}

## Function calls for testing

x <- matrix(1:4,nrow=2,ncol=2)
makeCacheMatrix(x)
inverse <- cacheSolve(x)
