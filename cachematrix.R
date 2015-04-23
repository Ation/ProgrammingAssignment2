## Coursera "R programming"
## Programming assignment 2

##
## makeCacheMatrix - creating caching matrix, that could save the inverse of
##                   itself to prevent calculating it next time it is required
makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        
        list(   set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## 
## cacheSolve - calculate the inverse of matrix. if it is present - result should be read from cache
## 


cacheSolve <- function(x, ...) {
        inverseX <- x$getInverse()
        if ( !is.null(inverseX) ) {
                return inverseX
        }
        
        matrix <- x$get()
        inverseX <- solve(matrix, ...)
        
        x$setInverse( inverseX )
        
        inverseX
}
