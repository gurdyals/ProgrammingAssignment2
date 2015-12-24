## Put comments here that give an overall description of what your
## functions do
#######################################################################
# function "makeCacheMatrix" returns a list of 4 functions
# namely {set, get, setInverse, getInverse}
# which operate on an Invertible Matrix "mtx2inv"
# take its input a Matrix (which is Invertible "mtx2inv")
# Inverted output matrix is "mtx"
# This Output Matrix "mtx" is CACHED
# if original Input "mtx2inv" Matrix has not changed
#######################################################################

# function "cacheSolve" takes input Matrix "mtx2inv" as its input
# and returns a Inverted Output Matrix "mtx" also this matrix is preserved
# in a different environment or CACHED for later use if required to save time
#######################################################################
# Another function Hilbert{Matrix} can be used to create an Invertible Matrix
# rather simulation of Hilbert{Matrix} will be used to create an Invertible Matrix
# for this assignment, which will create a symmitrix Matrix of size n by n
# WE can use following also instead of simulating "Hilbert" { library("Matrix") }

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

# n <- 7                     ##### These are just for testing purposes
# mtx2inv <- hilbert(n); mtx2inv ##### These are just for testing purposes
# mtx <- solve(mtx2inv); mtx     ##### These are just for testing purposes
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
    mtx2inv <<- y
    mtx <<- NULL
  }
  get <- function() mtx2inv
  setInverse <- function(inversed_mtx) mtx <<- inversed_mtx
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
  data <- x$get()
  inversed_mtx <- solve(data, ...)
  mtx <- x$setInverse(inversed_mtx) # set variable "mtx" for use by other functions
  mtx
}
#######################################################################

# Code after this line is for testing purpose and should not be considered
# part of the assignment
# Thanks
##### rm(list = ls())
## Change the value of "n" to test creation/inversion of Matrix
n <- 5
mtx2inv <- hilbert(n)
m1 <- mtx2inv
m1
func_invert <- function (mtx1, ...) solve(mtx1, ...)
inversed_mtx <- func_invert(m1)
inversed_mtx

mtx2inv
varM <- makeCacheMatrix(mtx2inv)
varM$mtx
cacheSolve(varM)

m2 <- varM$get()
m2
varM$set(m2)
varM$mtx2inv
varM$mtx
varM$getInverse()
cacheSolve(varM)
varM$getInverse()
mtx2inv <- varM$set(mtx2inv)
mtx2inv
inversed_mtx <- func_invert(m2)
inversed_mtx
varM$setInverse(inversed_mtx)
inversed_mtx