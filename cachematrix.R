## Similar to the given example, there is a cached value for the inverse
## If this cached value for the inverse is a NULL, then a new inverse value is calculated. 
## The cached value is assigned this inverse value as well

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   set the inverse of the matrix
# * getInverse     get the cached value (inverse of the matrix)
#

makeCacheMatrix <- function(x = matrix()) {
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # if matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # return stored matrix
        getMatrix <- function() {
                x
        }

        # cache inverse of matrix 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached inverse
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}


## This function calculates the inverse of the given matrix
## It also assigns it to the cached value in the vector list above

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the cached value
        inverse <- y$getInverse()
        # if cached value is not null return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # if cached value is null, calculate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
