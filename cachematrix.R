## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This file creates a list that have the following functions
## 1. set() function: initialize a matrix using 
## 2. get() function: return a matrix 
## 3. setInverse() function: return the cached inverse of a matrix if exists 
## 3. getInverse(): compute the inverse of a matrix, if the inverse matrix is not computed computed yet.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
		##initialize the matrix with set function, 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##obtain and return the matrix
        get <- function() x
        
        ##find the inverse and return the inverse matrix, if inverse of a matrix does not exist.
        setInverse <- function(Inverse) m <<- Inverse
        ##if inverse matrix exist, return the cached value
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## compute the inverse of the matrix created by makeCacheMatrix 
## if the inverse matrix exists, returns the cached value
## if not, uses solve() to find the inverse matrix and return it. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}

##sample code to call the above two functions
##initialize a special matrix , 
##
x<-makeCacheMatrix()
x$set
x$get() # returns a 1x1 matrix contains "NA"
x$getInverse() # return NULL
x$setInverse() # return erro message

temp<-matrix(c(1,2,2,1), nrow=2,ncol=2)
x$set(temp)

##first time, will call solve()function, and return the inverse matrix
cacheSolve(x)

##second time, will return the cached matrix
cacheSolve(x)

## show the original matrix
x$get()

## reset the matrix to new values
temp<-matrix(c(1,3,3,1), nrow=2,ncol=2)
x$set(temp)
x
x$get()
cacheSolve(x)
