##The following functions should create square matrix which is invertible,
##make the inverse of the matrix available in the cache environment,
##and cache the inverse of the matrix than computing it repeatedly.

makeCacheMatrix <- function(x = matrix()) {         ##It takes in the argument matrix
  inv <- NULL                                       ##Assumption: matrix supplied is always invertible
  set <- function(y) {                              ##set the value of the matrix
    x <<- y                                         ## <<- operator will help assign value from other environments too
    inv <<- NULL
  }
  get <- function() x                               ##get the value of the matrix 
  setinverse <- function(inverse) inv <<- inverse   ##set the value of inverse 
  getinverse <- function() inv                      ##get the value of inverse  
  list(set = set,                                   ##A list has been created
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()               ##Return a matrix that is the inverse of 'x' and assign it to 'inv'
  if (!is.null(inv)) {                  ##Find whether the calculation is made previously, to retrieve from cache memory
    message("getting cached data")      ##If the inverse is retrieved from cache, then this message will appear
    return(inv)                         ##inverse will be returned
  }
  matrix <- x$get()                     ##If no calculation exist in cache then,
  inv <- solve(matrix, ...)             ##calculate inverse martrix and 
  x$setinverse(inv)                     ##set the value in the cache
  inv
}

source("ProgrammingAssignment2.R")
mathimatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) ##A 2X2 matrix has been created
mathimatrix$get()                                 ##will print the matrix
mathimatrix$getinverse()                          ##will compute the inverse of matrix and
##this will give 'NULL' which means this has not been calculated earlier
cacheSolve(mathimatrix)                           ##This will give inverse matrix and store in cache memory
mathimatrix$getinverse()                          ##Now it will return inverse matrix and not 'NULL'
cacheSolve(mathimatrix)                           ##This will now show "getting cached data" and display the inverse matrix
cacheSolve(mathimatrix)                           ##This will again show "getting cached data" and display the inverse matrix



