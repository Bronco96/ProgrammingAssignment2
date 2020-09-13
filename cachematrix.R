## Below are two functions that are used to create a special object 
## that stores a numeric matrix and cache's its inverse

## This first function, makeCacheMatrix creates a special Matrix
## that can cache its inverse
#       1. set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of the inverse
#       4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <-function(y){
                x<<-y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function computes the inverse 
## of the special matrix returned by the above function
# However, it first checks to see if the inverse has already been calculated
# If so, it gets the inverse from the cache and skips the computation
# Otherwise, it calculates the inverse of the matrix 
# and sets the value of the inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}


#TESTING CODE, run in order to see how testmatrix$getInverse works after cached

testmatrix<- makeCacheMatrix(matrix(c(4,2,7,6), nrow = 2, ncol = 2))

testmatrix$get()
testmatrix$getInverse()

cacheSolve(testmatrix)
cacheSolve(testmatrix)

testmatrix$getInverse()
