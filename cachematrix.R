## Function Description: makeCacheMatrix()
## Function to create a special matrix
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <- y
  }
  get <- function() x
  setinv <- function(mInv){
    # We use the <<- operator to cache the values
    inv <<- mInv
  }
  getinv <- function() inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}
  
## Function Description: cacheSolve()
## This function retrives the inverse of the matrix from the cache.
## If it is not in cache, it re-computes the inverse and returns the result.
cacheSolve <- function(x,...){
  inv <- x$getinv()
  print(inv)
  if(!is.null(inv)){
    message("Getting the cached matrix")
    return(inv)
  }
  matx <- x$get()
  inv <- solve(matx, ...)
  x$setinv(inv)
  inv
}
# Testing the Code
x <- matrix(2:5,nrow=2,ncol=2)
mat <- makeCacheMatrix(x)
inverse <- cacheSolve(mat)
inverse <- cacheSolve(mat)
print(inverse)