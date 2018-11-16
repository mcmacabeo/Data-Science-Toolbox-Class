## R Programming Assignment 2: Lexical Scoping

## To create a special "matrix" object that can cache its inverse.
## 1. set the matrix; 2. get the matrix
## 3. set the inverse; 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
                I <- NULL
                set <- function(y) {
                        x <<- y
                        I <<- NULL
                }
                get <- function() x
                setInverse <- function(solveMatrix) I <<- solveMatrix
                getInverse <- function() I
                list(set = set, get = get,
                     setInverse = setInverse, getInverse = getInverse)

}

## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cache data")
                return(I)
        }
        ## Return a matrix that is the inverse of 'x'
        mat <- x$get()
        I <- solve(mat, ...)
        x$setInverse(I)
        I
}
