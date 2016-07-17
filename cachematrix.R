################ 
## Ashwin Kadle
################
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
################
makeCacheMatrix <- function(x = matrix()) {
  mat_inv <-NULL
  list (
    set = function(y){
      x<<-y
      mat_inv<<-NULL
    },
    get = function(){x},
    setInverse = function(inverse) {mat_inv <<- inverse},
    getInverse = function() {mat_inv}
  )
  
}

##########
## This function computes the inverse of the special "matrix" created by 
## function above. If the inverse has already been calculated
## then it should retrieve the inverse from the cache.If the matrix changes 
## you need to run cacheSolve function again
##########

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getInverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  mat <- x$get()
  mat_inv <- solve(mat, ...)
  x$setInverse(mat_inv)
  mat_inv
}
