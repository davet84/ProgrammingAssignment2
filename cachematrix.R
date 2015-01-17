## Put comments here that give an overall description of what your
## functions do

## special matrix creation function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){   #Set value of matrix
        x <<- y
        m <<- NULL
  }
    get <- function() x   #get value of matrix
    setmatrix <- function(solve) m<<- solve   #set value of inverse matrix
    getmatrix <- function() m     #get value of inverse matrix
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)

}


## computes inverse of special matrix in makeCacheMatrix function
cacheSolve <- function(x, ...) {
    #m <- x$getmatrix()
    m <- NULL
    if(!is.null(m)){    #check to see if matrix is not null
        message("getting cached data")
        return(m)
    }
    matrix <- x$get() 
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
        ## Return a matrix that is the inverse of 'x'


a<-makeCacheMatrix()
a$set(matrix(3:6,2,2))
cacheSolve(a)
a
print(a)
det(a)
solve(a)
cacheSolve(a)
solve(a)
