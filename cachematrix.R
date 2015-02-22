## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
       set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse
       )
}


## cacheSolve: calculates the inverse of the special "matrix" created with above function 
## IF inverse of the matrix has already been calculated. 
## THEN get the inverse from the cache and do not compute. 
## ELSE calculate inverse of matrix and set in cache using setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
