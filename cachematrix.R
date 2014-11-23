## there are 2 functions, one that creates the objetcs (matrix and function to inverse and cache that inverse
## and the other one to process those objetcs and display the 

## function to create the object matrix

makeCacheMatrix <- function(x = matrix()) { #create the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x     #get the value of the matrix
        setSolve <- function(solve) m <<- solve # #set the inverse of the Matrix
        getSolve <- function() m   #get the inverse of the matrix that is cached
        list(set = set, get = get,   # the list of objetcs created
             setSolve = setSolve,
             getSolve = getSolve)
}


## function that processes the inverse or just displays it if it is already cached
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getSolve()
        if(!is.null(m)) {    #if the inverse of the matrix is already cached we display the msg before printing it
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() # if it is not cached we process it before printing it
        m <- solve(matrix, ...)
        x$setSolve(m)
        m
}

