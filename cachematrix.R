## HW2 2 in R course

## Write a short comment describing this function
 ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse

}


 ## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
