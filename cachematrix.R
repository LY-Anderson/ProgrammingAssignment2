## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly.
## The following pair of functions cache the inverse of a matrix.


## The first function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing 4 functions to:
##        * set the value of the matrix
##        * get the value of the matrix
##        * set the value of the inverse matrix
##        * get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

            i <- NULL
            set <- function(y) {                   # can set another matrix_you_want_to_invert
                  x <<- y    
                  i <<- NULL
            }
            get <- function() x                               # returns x, the matrix_you_want_to_invert
            setinverse <- function(invert_m) i <<- invert_m   # caches the value of the inverted matrix in i for future reference
            getinverse <- function() i                        # returns i, NULL if mean never cached, inverted matrix if cached previously
            list(set = set, get = get,                        # make the list of functions
                 setinverse = setinverse,
                 getinverse = getinverse)
     
}


## The second function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the matrix in the cache via the setinverse function.

## Computing the inverse of a square matrix can be done with the
## solve function in R. For example, if X is a square invertible matrix,
## then solve(X) returns its inverse.

## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
            i <- x$getinverse()                  # get the mean if cached, otherwise returns NULL
            if(!is.null(i)) {                    # if the mean is not NULL, return the message and the mean
                  message("getting cached data")
                  return(i)
            }
            data <- x$get()                    # get the vector
            i <- solve(data, ...)              # calculate the mean of the vector
            x$setinverse(i)                    # cache the mean in m for future reference
            i                                  # return the mean, m  
}

#### TESTING
####
#### use this square matrix to test: j<-matrix(c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10),4,4)
####
#### to run this:
#### blah <- makeCachematrix(matrix_you_want_to_invert)
#### answer <- cacheSolve(blah)
#### answer2 <- cacheSolve(blah)

