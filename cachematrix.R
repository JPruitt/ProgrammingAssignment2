## Programming Assignment #2
## Pruitt

## makeCacheMatrix()
## Creates a special "matrix" object that can cache its inverse.
## Modified from makeVector()
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(cacheSolve) m <<- cacheSolve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve()
## Computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Modified from cacheMean()
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setinv(m)
        m
}

## Test Data from Discussion Boards
## myMatrix<-rbind(c(1,2), c(3,4))
## invMatrix<-solve(myMatrix) %*% myMatrix

## a <- makeCacheMatrix(myMatrix)

## a$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## a$getinv()
## NULL

## cacheSolve(a)
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## a$getinv()  
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## cacheSolve(a)
## getting cached data
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## a$set(invMatrix)
## a$getinv()
## NULL

## cacheSolve(a)
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## cacheSolve(a)
## getting cached data
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## a$get()
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## a$setinv(0)  
## a$getinv()
## [1] 0

## a$get()
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## cacheSolve(a)
## getting cached data
## [1] 0

## a <- makeCacheMatrix(myMatrix)
## a$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## cacheSolve(a)
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00

## cacheSolve(a)
## getting cached data
##       [,1]         [,2]
## [1,]    1 4.440892e-16
## [2,]    0 1.000000e+00
