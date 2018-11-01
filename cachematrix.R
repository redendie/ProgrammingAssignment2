## Use a closure to build caching capability and avoid calculating
## values already computed: in this case, the inverse of a matrix.


## The caching mechanism
makeCacheMatrix <- function(M = matrix()) {
        # The cache stores the matrix of interest and
        # the inverse of this matrix. No calculation happen
        # here: the inverse matrix is being stored in the
        # `inverse` variable through a setter function.
        M.inverse <- NULL
        set <- function(N) {
                M <<- N
                inverse <<- NULL
        }
        get <- function() M
        setinverse <- function(inverse.to.cache) M.inverse <<- inverse.to.cache
        getinverse <- function() M.inverse
        
        # Returns an interface to manage the state of the cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the inverse of a cache-enabled matrix. The caching logic
## is operated from here.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        # `inverse` is not null when the computation has already been done.
        # The cached value is returned.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # Otherwise, calculate the inverse here,
        # and use the setter function from `x` to store
        # the calculated inverse in the cache
        M <- x$get()
        inverse <- solve(M, ...)
        x$setinverse(inverse)
        
        # Return the inverse just calculated.
        inverse
}


