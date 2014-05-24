## Function Description: makeCacheMatrix()
## Function to create a special matrix
makeCacheMatrix <- function(x=matrix()){
  # initialize the inv variable as NULL
  inv <- NULL
  # Setter for the matrix variable 'x'
  set <- function(y){
    x <- y
  }
  # Getter for the matrix variable 'x'
  get <- function() x
  # Setter for the inverse variable 
  setinv <- function(mInv){
    # We use the <<- operator to cache the values
    inv <<- mInv
  }
  # Getter for the matrix variable 
  getinv <- function() inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}
  
## Function Description: cacheSolve()
## This function retrives the inverse of the matrix from the cache.
## If it is not in cache, it re-computes the inverse and returns the result.
cacheSolve <- function(x,...){
  # Here we obtain the inverse if it has been set
  inv <- x$getinv()
  # We check if we have obtained the inverse from the cache
  if(!is.null(inv)){
    message("Getting the cached matrix")
    return(inv)
  }
  # Control comes here as the inverse is not in the cache as it has not been calculated
  matx <- x$get()
  # We use solve to calculate the inverse
  inv <- solve(matx, ...)
  # Then we set the inverse into the cache
  x$setinv(inv)
  inv
}
# Testing the Code by creating a matrix and calling the two functions
x <- matrix(2:5,nrow=2,ncol=2)
mat <- makeCacheMatrix(x)
# Here inverse is not set in cache
inverse <- cacheSolve(mat)
# Here inverse is set in cache
inverse <- cacheSolve(mat)
print(inverse)
