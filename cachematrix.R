## Richard Sprague, sprague@outlook.com 
## Programming Assignment # 2
## https://github.com/richardsprague/ProgrammingAssignment2


## example use of this function:
## sampleMatrix <- matrix(c(1,2,3,4,5,5,9,8,9),3,3)
## m <- makeCacheMatrix(sampleMatrix)
## cacheSolve(m)
#### (returns a matrix that is the Solve, i.e. inverse of sampleMatrix)
## cacheSolve(m)
## (returns the same matrix as the line above, but without re-calculation)

## Creates a new matrix that can be used with the cacheSolve function below
## Since the normal R 'solve' function can be expensive, a cacheMatrix will
## a cacheMatrix will return a cached version of a solve calculation if one exists.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(mean) m <<- mean
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## returns the result of applying the built-in R 'solve' function 
##  to a matrix that was created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # from the makeCacheSolve function above
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)  # computes the inverse using R's built-in function
        x$setsolve(m)
        m 
}

# Other notes:
# uses the built-in R function 'solve'
# if a is a matrix
# and x = solve(a)
# then a * x = the identity matrix