## 
## Storing and then calculating the inverse of a matrix (which is cached)
## An example of use is shown at the bottom of the script
##


## makeCacheMatrix stores a matrix which should be square and inversible.
## The return value is a list object with helper functions for cached matrix 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve take an argument that is the list object containing the stored matrix.
## After checking that a valid formed matrix is passed in the a check is made to see
## if an inverse matrix has been stored, otherise the inverse matrix is calculated.

cacheSolve <- function(x, ...) {
      if(!is.recursive(x))
      {
            message("use the return value of makeCacheMatrix passing in a square matrix") 
            return(NULL)
      }
      data <- x$get()
      if(!is.matrix(data))
      {
            message("use the return value of makeCacheMatrix needs to contain in a matrix") 
            return(NULL)
      }
      else if(nrow(data)!=ncol(data))
      {
            message("a squre matrix is required") 
            return(NULL)
      }
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }

      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      #Returns a matrix that is the inverse of 'x'
}


##Example of functions in use
#> m1<-rbind(c(2, 1, 0), c(2, 0, 0), c(2, 0, 1))
#> cm<-makeCacheMatrix(m1)
#> cacheSolve(cm)
#[,1] [,2] [,3]
#[1,]    0  0.5    0
#[2,]    1 -1.0    0
#[3,]    0 -1.0    1
#> 
#> cacheSolve(cm)
#getting cached data
#[,1] [,2] [,3]
#[1,]    0  0.5    0
#[2,]    1 -1.0    0
#[3,]    0 -1.0    1
#> 
#> m2<-rbind(c(3, 1, 0), c(3, 0, 0), c(3, 0, 1))
#> cm$set(m2)
#> cacheSolve(cm)
#[,1]       [,2] [,3]
#[1,]    0  0.3333333    0
#[2,]    1 -1.0000000    0
#[3,]    0 -1.0000000    1
#> cacheSolve(cm)
#getting cached data
#[,1]       [,2] [,3]
#[1,]    0  0.3333333    0
#[2,]    1 -1.0000000    0
#[3,]    0 -1.0000000    1
#> 