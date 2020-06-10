# the task given is to create a special "matrix" object that can cache its inverse.
# for this we will be using solve(x) a function in R to return inverse value of x

##first we create a special vector called makeCacheMatrix
## the first chunk of the function is to set the value of the vector line 11-16
## the second variable get is assigned to get the value of the vector makeCacheMatrix.
## setInverse is a function to set the value of inverse.
## then, after that we assigned the value of inverse to a new variable.
##this list function is to construct the value calculated above.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y){
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInverse <- function(createMatrix) invert <<- createMatrix
        getInverse <- function() invert
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function is to compute the special matrix we have created above.
## if the inverse has been calculated it will return the value rather than calculate it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getInverse()
        if(!is.null(invert)){
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data) #inverse the solution.
        x$setInverse(invert)
        invert      
}
