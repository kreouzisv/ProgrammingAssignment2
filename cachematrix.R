## The following code is dedicate to assignment 2 on Coursera's data science course, allowing for effective cache creation of the inverse of a matrix followed by computation of the inverse derived by it. 
## 

## Creation of a cache of the inverse of the input matrix, x 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Computation of cache matrix's invrerse derived from function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    	inv <- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      

}
