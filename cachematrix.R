## Written by Colin King for "R Programming" taught by Roger Peng from Johns 
## Hopkins University via Coursera.

## Structure of code influenced by and modified from sample code 
## provided in-class.

## makeCacheMatrix and cacheSolve will compute the inverse of a given matrix 
## and cache the result. On further calls to cache a given matrix, the 
## functions will check to see if the inverse has already been calculated in 
## order to prevent repeating hefty calculations.

## makeCacheMatrix creates and returns a special matrix that contains 
## four functions:
## 1. set the value of the matrix (x)
## 2. get the value of the matrix (x)
## 3. set the value of the inverse of the matrix (i)
## 4. get the value of the inverse of the matrix (i)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will check to see if the inverse for the passed in matrix has 
## already been calculated. If so, it returns this cached value. If not, it
## calculates the inverse via the solve function, saves this value and returns
## the newly calculated inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
