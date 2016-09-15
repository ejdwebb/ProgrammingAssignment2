## Enables result of inverting a matrix to be cached

## Initializes result as a cachable matrix
## set sets matrix
## get returns current value of matrix
## setinverse stores matrix inverse
## getinverse returns currently cached matrix inverse
makeCacheMatrix <- function( matrix = double() ){
        inverse <- NULL       
        set <- function( newmatrix ){
                matrix <<- newmatrix
                inverse <<- NULL
        }
        get <- function() matrix
        setinverse <- function(newinverse) inverse <<- newinverse
        getinverse <- function() inverse
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns cached matrix inverse or calculates inverse if nothing currently in cache
## Set messaging = FALSE to turn off messages
cacheSolve <- function( matrix, ..., messaging = TRUE ){
        inverse <- matrix$getinverse()
        if( !is.null(inverse) ){
                if( messaging ){
                        message( "Returing cached value" )
                }
                return(inverse)
        }
        if( messaging ){
                message( "Calculating inverse, this may take some time for large matrices")
        }
        newmatrix <- matrix$get()
        inverse <- solve( newmatrix, ... )
        matrix$setinverse( inverse )
        inverse
}