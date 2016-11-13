## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## @x:a list invertible matrix
  ## 1. set the matrix
  ## 2. get the matrix
  ## 3. set the inverse
  ## 4. get the inverse
  ## input cacheSolve
  
  inv <- NULL
  set <- function(y) {
    ## <<- for assign a value to an object an environment 
    ## diffrent from current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## @x :output of makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  ## if the inverse had alredy be calculated
  if(!is.null(inv)) {
    ## get it from the cache and skip
    message("getting cached data")
    return(inv)
  }
  ## calculate the inverse
  matrix.data <- x$get()
  inv <- solve(matrix.data,...)
  ## set the value of inverse in the cache
  x$setinv(inv)
  inv
  
}
