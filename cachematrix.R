##############################################################################
## Put comments here that give an overall description of what your
## functions do : W.r.t. makeCacheMatrix function
##############################################################################
# Purpose: R programming assigments for a Coursera Course
#
# Student: Mike C
#
# Date: 23-Jun-16
#
# Requirement: Function to creates a special "matrix" object that can cache 
#              its inverse.
#
# Parent: makeVector.R (by Coursera) was modified from a vector caching 
#         script to a matrix caching scrips which caches a matrix which is 
#         the inverse matrix of a inputted matrix.
#
############ How is this script used ########################################
#> source("makeCacheMatrix.R")
#> inputObj <- makeCacheMatrix(matrix(c(1,3,2,4), 2, 2))
#############################################################################
########### Review the input for the matrix(c(1,3,2,4), 2, 2) ###############
#> inputObj$get()
#       [,1] [,2]
#[1,]    1    2
#[2,]    3    4
# Acording to http://www.mathwords.com/i/inverse_of_a_matrix.htm 
# the inverse matrix is 
#      [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#############################################################################
############# Write a short comment describing this function#################
# The function call is called through the assignment to a variable, the 
# arguments of the function decribes the a square matrix (in order for code
# to work correctly). 
#
# Inside the function call, is the initiation on some local and global 
# variables. A global variable will evenutally hold the cached output of the 
# program.
#
# the 2nd half of the script creates aspecial vector or list that contains a 
# function to
#
# set the values in the matrix
# get the values in the matrix
# set the values in the inverse matrix
# get the values in the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        cachedObj <- NULL          # Initiate the variable cachedObj
        set <- function(y){
                x <<- y            # x is set to equal  y and x is a global 
                                   # varible
                cachedObj <<- NULL # cachedObj is set to a global variable
                                   # set to Null
        }
        get <- function() x        # x is not an argument of this lines  
                                   # function call, so a series of 
                                   # enviroments will be searched to find 
                                   # an appropriate value for x
        setcachedObj <- function(inverse) cachedObj <<- inverse
        getcachedObj <- function() cachedObj  # cachedObj is not an argument  
                                              # of this lines function call  
                                              # so a series of enviroments  
                                              # will be searched to find an 
                                              # appropriate value for 
                                              # cachedObj
        list(set=set, get=get,
             setcachedObj=setcachedObj,
             getcachedObj=getcachedObj)
}

##############################################################################
## Put comments here that give an overall description of what your
## functions do : W.r.t. makeCacheMatrix function
##############################################################################
############################################################################
# Purpose: Coursera Course called R programming assigments
#
# Student: Mike C
#
# 23-Jun-16
#
# Requirement: Function to computes the inverse of the special "matrix" 
#              returned by previous function makeCacheMatrix. If the 
#              inverse has already been calculated (and the matrix has 
#              not changed), then this script will retrieve the inverse
#              matrix from the cache.
#
# Parent: cachemean.R (by Coursera) was modified from a vector (mean) 
#         retrivial from cache script to a matrix retrivial cache scrips 
#         which retrives from cache a matrix which is the calculated 
#         inverse matrix of a inputted matrix.
#
######## How is this script used ##########################################
#> source("cacheSolve.R")
#> cacheSolve(inputObj)
###########################################################################
############# Write a short comment describing this function#################
# The function call below is called with a local argument, that was the 
# variable in which the above script was call through. 
#
# This function call retrived the global variable that hold a inverse matrix
# and if it is not a null then, echos it to the screen with an message.
#
# If the golbal variable is null, then the inverse is calculated and echoed 
# to to the screen
cacheSolve <- function(x=matrix(), ...) {
        outputObj <- x$getcachedObj()
        if(!is.null(outputObj)){                # if cached then use it
                message("getting cached data")  # Echo message to screen
                return(outputObj)               # Echo output to screen
        }
        matrix <- x$get()
        outputObj <- solve(matrix, ...)         # function call solve()
                                                # provides the inverse of
                                                # a square matrix
        x$setcachedObj(outputObj)               
        outputObj                               # Echo output to screen
}

