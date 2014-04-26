## Function Description: makeCacheMatrix()
## Function to create a special matrix
makeCacheMatrix <- function(x = matrix()) {
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

## Function Description: cacheSolve()
## This function retrives the inverse of the matrix from the cache.
## If it is not in cache, it re-computes the inverse and returns the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- NULL
  
  ## Calling the fuction to set the value in the cache
  x$set_inv()
  
  ## Getting the value from the cache
  inv_matrix <- x$get_inv()
  if(!is.null(inv_matrix)) {
    message("Retrived the inverse Matrix from cache")
    return(inv_matrix)
  }
  
  ## Could not get the inverse matrix from the cache
  inv_matrix
}

## Function calls for testing
x <- matrix(2:5,nrow=2,ncol=2)
mat <- makeCacheMatrix(x)
inverse <- cacheSolve(mat)
print(inverse)