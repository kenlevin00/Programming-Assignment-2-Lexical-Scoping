## This procedure uses cache to store in memory an inverse of a matrix, 
## which is a memory "heavy" action. If the inverse is not found in cache, it calculates it.


## This function creates setters and getters (or in simple terms it create functions
## that can later identify whether the input argument is stored in cache and retrives it)

makeCacheMatrix <- function(x = matrix()) {         ##define the function and initialize x
        s <- NULL                                   ##initialize s to NULL
        set <- function(y) {                        ##define set() function (setter for x)
                x <<- y                             ##assign the input argument to the parent environment
                s <<- NULL                          ##assing NULL to s (reset to s)
        }
        get <- function() x                         ##define the getter for argument x
        setsolve <- function(solve) s <<- solve    ##define the setter for the argument s
        getsolve <- function() s                    ##define the getter for the argument s
        list(set = set, get = get,                  ##assign the setter and getters to a list in the parent environment
             setsolve = setsolve, getsolve = getsolve)
}


## this function checks if the argument is already stored in memory and retrieves is, or if not, calculates the inverse of the argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()                           ##retrieve the inverse (solve) of x (if exists)
        if(!is.null(s)) {                           ##check whether s has a value (not NULL)
                message("getting cache data")       ##message
                return(s)                           ##return the value of s
        }                                           ##if s in NULL
        data <- x$get()                             ##get the matrix from the input object                         
        s <- solve(data, ...)                       ##calculate the inverse (solve)
        x$setsolve(s)                               ##set the inverse
        s                                           ##return the inverse to the parent environment
}
