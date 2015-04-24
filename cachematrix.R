## Functions to verify if inverse of a matrix has been previously
## calculated. If so, get it from cached inverse. Otherwise, proceed with 
## calculation

# 

## Function makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the inverse of the matrix
## 4.	get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse
## in the cache via the setinverse function

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


## TEST BASIC CACHING PROVIDED BY COURSE TA IN FORUM
## Uncomment next 7 lines  to verify that above functions work

# n <- 3
# mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
# matCached <- makeCacheMatrix(mat)
# matSolved1 <- cacheSolve(matCached)
# matSolved2 <- cacheSolve(matCached)
# if (!identical(matSolved1, matSolved2))
#   message("Cached version does not match solved version")




