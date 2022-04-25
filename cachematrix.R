## Caching the Inverse of a Matrix 

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function checks to see if the inverse of the special matrix has been calculated. If it has then it retrieves the inverse from cache, if not then it computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}

#Use the function
matrix <- makeCacheMatrix()
#set matrix value
matrix$set(matrix(1:4, 2, 2))
#get matrix
matrix$get()

#get the inverse
inv=solve(matrix$get())
matrix$setInverse(inv)
matrix$getInverse()