##Assignment: Caching the Inverse of a Matrixless 
##Matrix inversion is usually a costly computation and there may 
##be some benefit to caching the inverse of a matrix rather than 
##compute it repeatedly (there are also alternatives to matrix 
##inversion that we will not discuss here). Your assignment is 
##to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getInverse()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  mat <- x$get()
  inv_mat <- solve(mat, ...)
  x$setInverse(inv_mat)
  inv_mat
}
