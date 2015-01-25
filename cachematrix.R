## Create the cache of a matrix and return inverse matrix if already computed previously
## Assumption: matrix supplied is always invertible.
## Function to save compute time

## Create a matrix and cache the inverse


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## Calculate the inverse and cache the saved value
## If inverse is calculated for static matrix, retrive cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #Solve the inverse of a square matrix
        m <- solve(data, ...)
        x$setmean(m)
        m

}
