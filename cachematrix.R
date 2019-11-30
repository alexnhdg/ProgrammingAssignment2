# R Programming: Week 3, Programming Assignment 2
# Alexander de Groot
# November 30, 2019


# This program calculates the inverse matrix of a user-provided invertible
# matrix. Calculating the inverse of a matrix can be a computationally
# intensive process. This program utilizes the lexical scoping
# feature of R to cache, i.e. store, the computed inverse matrix so that it
# does not have to be re-calculated if it is needed again in future parts of
# the program.


# The function 'makeCacheMatrix' stores a matrix and its inverse. It also 
# creates a special list object containing functions that 'cacheSolve' needs
# in order to retrieve (if inverse matrix is already cached) or calculate (if
# inverse matrix has never been calculated) the inverse of the user-provided
# invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setim <- function(inverse_matrix) im <<- inverse_matrix
        getim <- function() im
        list(set = set, get = get,
             setim = setim, getim = getim)
}


# The 'cacheSolve' function computes the inverse of the object returned
# by 'makeCacheMatrix' above. If the inverse has already been calculated (and the
# matrix has not changed), then 'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
}



# Example program execution
mymatrix <- matrix(c(4, 2, 7, 6), 2, 2) # Create an invertible matrix
myCacheMatrix <- makeCacheMatrix(mymatrix) # Stores inverible matrix and creates functions needed to retrieve/calculate inverse
cacheSolve(myCacheMatrix) # First iteration of cacheSolve calculates the inverse matrix
cacheSolve(myCacheMatrix) # Second iteration of cacheSolve accesses cache to retrieve the already calculated inverse matrix
