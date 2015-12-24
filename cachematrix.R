## Put comments here that give an overall description of what your
## functions do
#######################################################################

# function "makeCacheMatrix" returns a list of 4 functions
# namely {set, get, setInverse, getInverse}
# which operate on an Invertible Matrix "mtx2inv"
# take its input a Matrix (which is Invertible "mtx2inv")
# Inverted output matrix is "mtx"
# This Output Matrix"mtx" is CACHED
# if original Input "mtx2inv" Matrix has not changed

# function "cacheSolve" takes input Matrix "mtx2inv" as its input
# and returns a Inverted Output Matrix "mtx" also this matrix is preserved
# in a different environment or CACHED for later use if required to save time

#######################################################################
# Another function Hilbert{Matrix} will be used to create an Invertible Matrix
# rather simulation of Hilbert{Matrix} will be used to create an Invertible Matrix
# for this assignment, which will create a symmitrix Matrix of size n by n
# WE can use following also instead of simulating "Hilbert" { library("Matrix") }

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# n <- 7                     ##### These are just for testing purposes
# mtx2inv <- hilbert(n)      ##### These are just for testing purposes
# mtx2inv                    ##### These are just for testing purposes
# mtx <- solve(mtx2inv)      ##### These are just for testing purposes
# mtx                        ##### These are just for testing purposes
# round(mtx %*% mtx2inv, 3)  ##### These are just for testing purposes
#######################################################################
## Write a short comment describing this function
# makeCacheMatrix function takes a Matrix as an argument
# returns a list of four functions {set, get, setInverse, getInverse}
# which can operate this Matrix
# This refers to matrix "mtx" which is not in its own scope(environment)
# "mtx" is local to function CacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setInverse <- function(mtx2inv) mtx <<- solve(mtx2inv)
  getInverse <- function() mtx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#######################################################################
## Write a short comment describing this function
# CacheSolve function takes a Matrix as Input (This Matrix is Invertible Matrix)
# it returns CACHED Inverted Matrix if it is avilable otherwise it computes
# Inverted Matrix of Input Matrix "mtx2inv" and returns Inverted Matrix "mtx"
# and keeps a copy of Inverted Matrix "mtx" 
# locally (local scope/environment of this function)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtx <- x$getInverse()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  ##### data <- x$get()  # This code is not required thus commented
  mtx2inv <- x$get()
  ##### mtx <- solve(data, ...)  # This code is not required thus commented
  mtx <- x$setInverse(mtx2inv)
  mtx
}
#######################################################################

# Code after this line is for testing purpose and should not be considered
# part of the assignment
# Thanks

##### rm(list = ls())

## Change the value of "n" to test
n <- 5
mtx2inv <- hilbert(n)
mtx2inv
solve(mtx2inv)

varM <- makeCacheMatrix(mtx2inv)
cacheSolve(varM)

varM$get()
varM$set(mtx2inv)
varM$getInverse()
cacheSolve(varM)
varM$getInverse()
varM$setInverse(mtx2inv)