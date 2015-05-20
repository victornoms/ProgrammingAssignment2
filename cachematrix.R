## The first function creates a special "matrix" object that stores a matrix
## and can cache the inverse of that matrix. The second function computes the
## inverse of the special "matrix" you passed and then caches it in the special
## matrix via $getinverse(i).

## The 1st function creates a special "matrix" object that essentially
## consists of a list containing a function that does four things:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

## The second thing, get(), returns the matrix when you run x$get(), assuming
## that you assigned the special "matrix" object to x (or another variable).
## The third thing, setinverse(), is used by the 2nd function; the cacheSolve()
## function computes the inverse of the special "matrix", assigns the inverse
## to the variable i, and then caches i in the special "matrix" object via
## x$setinverse(i). (You could theoretically manually set the inverse to a
## value like 10 by running x$setinverse(10). You shouldn't do this, though;
## it's not what the functions are meant to do!) The fourth thing,
## getinverse(), returns the inverse when you run x$getinverse().

## Note: You need to assign makeCacheMatrix(matrix) to an actual variable for
## both of these functions to work properly! If you run makeCacheMatrix(matrix),
## cacheSolve(makeCacheMatrix(matrix)), and then
## makeCacheMatrix(matrix)$getinverse(), it'll return a NULL value (because
## the inverse never got cached). Instead, assign the first function's special
## "matrix" object to a variable by running something like
## x <- makeCacheMatrix(matrix). After that, you can run cacheSolve(x) and then
## x$getinverse().

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The 2nd function computes the inverse of the matrix stored in the special
## "matrix" object from the first function. If the special "matrix" object 
## already has a stored value (it should be the inverse) in x$getinverse(),
## then this function will return a message ("getting cached data") and return
## the stored inverse. If the special "matrix" object does not have a stored
## value for x$getinverse(), then this function will calculate the inverse of
## the special "matrix" via solve(), assign the inverse to i, cache i in the
## special "matrix" object via x$setinverse(i), and then return the inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
