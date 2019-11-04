## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()) { ## will define the argument with default mode of "matrix"
    inv <- NULL                             ##define the set function to assign new
    set <- function(y){                     ##value of matrix in parent environment
      x <<- y                               ##value of matrix in parent enviroment
      inv <<- NULL                          ## if there is a new matrix, reset inv to null         
    }
    get <- function() x                     ## define the get function/returns value of the matrix argument
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }

}


## Write a short comment describing this function
  
## This function Returns a matrix that is the inverse of 'x'
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    ## If the inverse has already been calculated (and the matrix has not changed),
      ## then cacheSolve is going to retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
       
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
  }
  ##insert the code for the test
