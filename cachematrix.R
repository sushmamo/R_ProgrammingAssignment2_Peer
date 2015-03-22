## cachematrix.R
##
## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.

## There are two main functions created in this file
## 1. makeCacheMatrix
## 2. cacheSolve

## Please see the notes at the end for 
## Usage, Assumptions and Errors

makeCacheMatrix <- function(x = matrix()) {

## Input to the function: matrix named x
## Output of the function: list containing elements  
## set, get,setInverse and getInverse
## which are functions with the same names

## What the function does 
## When called for the first time as "M1 <- makeCacheMatrix (m1)"
## Step 1 
##	Set the cachememory for storing the inverse matrix as NULL, 
##	in the parent environment
## Steps 2 to 5: 
##	Defines the functions set, get, setInverse and getInverse
## Step 6: 
##	Creates a list with elements set, get, setInverse and getInverse
##	the contents of which are the definitions created in Steps 2-5

#Step1:
	cachedInverse <- NULL

#Step2:
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	}
#Step3:
	get <- function() x
#Step4:
	setInverse <- function(inverse) cachedInverse <<- inverse
#Step5:
	getInverse <- function() cachedInverse

#Step6:
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

## Input to the function: 
## cacheMatrix Object x (or M1, M2 in the usage examples) etc created using makeCacheMatrix function
## Output of the function: Inverse of the matrix which was passed to makeCacheMatrix function while creating the cacheMatrix Object

## What the function does: 
## Step 1: 
##	Executes x$getInverse() [or M1$getInverse() or M2$getInverse() in example cases]
##	ie it will look at the element named getInverse in the list x/M1/M2
##	it will execute the code of getInverse() which is defined in makeCacheMatrix function
##	if it is NULL, it means this is the first time you are inversing the matrix
##	if it is Not NULL, it will return the inverse matrix that is already cached in variable, invFunc
##	A message "getting cached data is displayed, if you are using the cached inverse, just to let you know

## Steps 2 to 5: 
##	get the input matrix using get() function, (which is defined in makeCacheMatrix) 		
##	derive th einverse of input matrix and assign it to variable invFunc
##	save invFunc in cache using setInverse function (which is defined in makeCacheMatrix), for future use		
##	return the inverse matrix

#Step1
	invFunc <- x$getInverse()
	if(!is.null(invFunc)) {
		message("getting cached data")
		return(invFunc)
	}

#Steps 2 to 5
	data <- x$get()
	invFunc <- solve(data, ...)
	x$setInverse(invFunc)
	invFunc
}

## Usage:
## Before calling this function, you need to create the matrix you want to invert, say m1 as follows:
##  	m1 <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## Next you pass m1 as the argument of makecacheMatrix 
## and store the output list in M1 as follows
##  	M1 <- makeCacheMatrix(m1)
## Whenever you want the inverse of m1, call cacheSolve function as follows:
##  	cacheSolve(M1)

## When you call cacheSolve for the first time, the inverse is derived by the getInverse function and is saved in cache
## Subsequently, whenever you call cacheSolve to get inverse of m1, it does not actually solve it
##instead it returns the inverse which is stored in cache memory, in "cachedInverse" variable

# Now you can create another matrix, m2, For example
#  	m2<-matrix(c(1,2,2,2,2,2,2,2,1),3,3)
# Then you pass m2 as the argument of makeCacheMatrix 
# and store the output list in M2 as follows
#  	M2 <- makeCacheMatrix(m2)
# Whenever you want the inverse of m2, call cacheSolve function as follows:
#  	cacheSolve(M2)

## Assumptions and Errors:
## Some matrices are not invertable. 
## Input is an invertale matrix
## If it is not invertale matrix, expect an error message as follows,
## when you call cacheSolve function:
## "Error in solve.default(data, ...) : 
## pack routine dgesv: system is exactly singular: U[2,2] = 0" 
