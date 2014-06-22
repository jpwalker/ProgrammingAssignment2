## makeCacheMatrix creates a list of functions which a manipulates a matrix 
## in this function's frame.

## cachSolve takes an object created by makeCacheMatrix and checks the invserse.
## If it is NULL then it sets the inverse matrix and returns the value.

## test function tests the two above functions.

## Function defines and returns a matrix-like object with a list of functions 
## which can manipulate a matrix-like object.

makeCacheMatrix <- function(x = matrix()) {
    ## Set inverse to NULL
    inverse <- NULL
    setmtx <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getmtx <- function() x 
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = setmtx, get = getmtx, setinverse = setinv, getinverse = getinv)
}

## Function returns the inverse of a matrix-like object x and caches 
## the result if necessary. Optional parameters are passed to Solve

cacheSolve <- function(x, ...) {
    ## Return and cache a matrix that is the inverse of 'x'
    ## Check if inverse already exists.
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("Retrieving inverse matrix")
    }
    else {
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
    }
    inverse
}

## Test function which tests the above funtions
test <- function() {
    mtx <- array(c(4, 5, 5, 1) , dim = c(2, 2)) ## Create Matrix
    mtxobj <- makeCacheMatrix() ## Create a Cache Matrix object
    mtxobj$set(mtx) ## Set the matrix in the Cache Matrix object
    print(mtxobj$get()) ## Print the matrix just set
    print(mtxobj$getinverse()) ## Gets the inverse of the Cache Matrix object (NULL)
    print(cacheSolve(mtxobj))
    print(mtxobj$getinverse()) ## Gets the inverse after cacheing
    mtxobj$setinverse(NULL)
}
