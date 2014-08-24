## Below are two functions that are used to create a special object that
## stores any nonsingular matrix and caches its inverse.

## The function makeCacheMatrix creates a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function () x
        setmatrixinverse <- function (matrixinverse) matinv <<- matrixinverse
        getmatrixinverse <- function() matinv
        list(set = set, get = get,
        setmatrixinverse = setmatrixinverse,
        getmatrixinverse = getmatrixinverse)
}

## The function cachesolve calculates the inverse of the matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setmatrixinverse function.

cacheSolve <- function(x, ...) {
        matinv <- x$getmatrixinverse()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        matrix <- x$get()
        matinv <- solve(matrix, ...)
        x$setmatrixinverse(matinv)
        matinv
}
