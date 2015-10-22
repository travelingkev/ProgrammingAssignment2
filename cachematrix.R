## Put comments here that give an overall description of what your
## functions do

## This function creates a cache matrix record with the functions:
## 	set, get, setinverse and getinverse
## 

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL  #initializes calculated inverted matrix as NULL
    set <- function(y) { # sets x to a new matrix value and resets the inverted_matrix
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x  #returns the matrix x
    setinverse <- function(solved_inverse) inv_m <<- solved_inverse  # stores the calculated inverted matrix for the function
    getinverse <- function() inv_m # returns the previously stored inverted matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks the cacheMatrix to see if it already has it's inverse calculated.  
## If not, it calculates it, updates the cache matrix, and returns the inverted matrix

cacheSolve <- function(x, ...) {
    inv_m <- x$getinverse() # retrieves current calculated version of the inverse from the cache matrix
    if (!is.null(inv_m)) { # if it's already been calculated, 
        message("using cached matrix inverse")   # it prints a message,
        return(inv_m)    # returns the pre-calculated inverse and exits the function
    }
	# if the matrix inverse hasn't already been calculated
    m <- x$get()  # retrieves the matrix from the CacheMatrix
    inv_m <- solve(m)  # creates the inverse of that matrix
    x$setinverse(inv_m) # updates the CacheMatrix with the new calculated value
    inv_m #returns the matrix inverse
}
