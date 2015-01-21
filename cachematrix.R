## Author:  Marion A Granich RN
## Date: 01/16/2015
## Programming Assigment 2
## Description: Costly operations can be cached.  The following two 
## functions will calculate and inverse the matrix 
## and cache the results for later use.  They are both 
## Modeled after the mean example written by R D Peng in the 
## homework assignment instructions.

## makeCacheMatrix
## This function produces a list of functions to get an set the value 
## of a matrix, and get and set the inverse of the matrix.  The final
## return value of this function is the list.

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Write a short comment describing this function

## This function uses solve() fuction to obtain the inverse of 
## the matrix,  but first checks to see if the inverse exists in
## cache and if it does, will return the cache value,  if not, will
## calculate the inverse and return it.   
## Per the instructions, we assume the matrix is always invertible.
cacheSolve <- function(x, ...) 
{
  i <- x$getinv()
  
  if(!is.null(i))  #Check to see if the inverse exists  
  {
    message("getting cached data.")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
## Some Test Results
## > x = rbind(c(1, 4, 3), c(3,7,1), c(2,1,4))
## > x
## [,1] [,2] [,3]
## [1,]    1    4    3
## [2,]    3    7    1
## [3,]    2    1    4
## > m <- makeCacheMatrix(x)
## > m$get()
## [,1] [,2] [,3]
## [1,]    1    4    3
## [2,]    3    7    1
## [3,]    2    1    4
## > cacheSolve(m)
## [,1]        [,2]       [,3]
## [1,] -0.5869565  0.28260870  0.3695652
## [2,]  0.2173913  0.04347826 -0.1739130
## [3,]  0.2391304 -0.15217391  0.1086957
## > cacheSolve(m)
## getting cached data.
## [,1]        [,2]       [,3]
## [1,] -0.5869565  0.28260870  0.3695652
## [2,]  0.2173913  0.04347826 -0.1739130
## [3,]  0.2391304 -0.15217391  0.1086957
