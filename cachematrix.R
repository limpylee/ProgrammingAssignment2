## makeChacheMatrix creates a list containing a function to
## 1.set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function (inverse) m <<- inverse
  getinverse <- function () m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse
## has already abeen calculated (and the matrix has
## not changed), then the cachesolve should retrieve
## the inverse of the cache.

## assumes matrix supplied is invertible
cacheSolve <- function (x, ...){
  inv <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
      return(m)
  }
  data <-x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
