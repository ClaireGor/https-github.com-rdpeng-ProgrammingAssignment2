## The following pair of functions cache the inverse of a matrix.


## This function creates an object of type list which stores two things - the original matrix and the inverse matrix (initilially
## set to NULL). It also has two functions to read and two functions to change the values it has stored. The object created is 
## accessed by the second function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes the object created by makecacheMatrix, checks whether "m" is Null, if yes, then it calculates the 
## inverse of the matrix, x, and stores it in m. On subsequent passes, m will no longer be NULL and so the message "getting
## cached data" is returned along with "m".

cacheSolve <- function(x, ...) {
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m         ## Return a matrix that is the inverse of 'x'
}
