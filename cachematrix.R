## This programme creates special objects that improves the acquisition speed of an inverted matrix
## by caching the result of the matrix inversion.
## An inverted Matrix result is very probable to be used frequently during computations with 
# other Matrices so the special Matrix object is extended to cache its inverted result.

##-----------------------------------------------------------
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInversed <- function(i) inversed <<- i
  getInversed <- function() inversed
  list(set = set, get = get,
       setInversed = setInversed,
       getInversed = getInversed)
}


## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix  above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## it tries to recover a possible inversed matrix from the cache
  inversed <- x$getInversed()
  
  ## in case that is available, returns the matrix to the user printing an informative message
  ## on the console
  
  if(!is.null(inversed)) {
    message("getting cached inversed matrix")
    return(inversed)
  }
    
  data <- x$get()
  inversed <- solve(data)

  ## It caches/stores the inversed matrix  
  
  x$setInversed(inversed)

  ## Return a matrix that is the inverse of 'x'
  inversed
  
}

# Trial run
#-----------

# m <- matrix(c(1,0,5,2,1,6,3,4,0), 3,3)
# > m
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# 
# 
# > solve(m)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# 
# > mcm<-makeCacheMatrix(m)
# > cacheSolve(mcm)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# 
# > cacheSolve(mcm)
# getting cached inversed matrix
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1





