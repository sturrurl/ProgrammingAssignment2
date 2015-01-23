## These functions take a matrix and convert the matrix to its inverse value.
## To save calculation time the inverse is "saved" and if the same conversion is required a 2nd time the 
## "saved" version is simply shown and the calculation is skipped.
 
## This function takes a matrix and initializes a special matrix with functions available to render the results
##  set - sets the initial matrix
##  get - retrieves the inital matrix
##  setinverse - stores the calculated inverse of the matrix (solve)
##  getinverse - retreives the inverse if previously stored via the setinverse
makeCacheMatrix <- function(x = matrix())
	{ 
	m <- NULL
	## Store Original Matrix
	set <- function(y)
		{
		x <<- y
		m <<- NULL
		}

  	## Retreive Original Matrix
	get <- function() x

	## Create and Cache Inverse Matrix
	setinverse <- function(solve) m <<- solve

	## Retreive Inverse Matrix    
	getinverse <- function() m

	## "Methods" for Hybrid Matrix                  
	list	(set = set, get = get,
       	setinverse = setinverse,
       	getinverse = getinverse
		)

	} 

 
## When supplied with a special Matrix defined by the makeCacheMatrix function this
## function will first see if the inverse has already been calculated. If so nothing happens.#
## If the inverse has not been calculated it is calculated and stored. So if called again there is no need to
## recalculate the inverse.
cacheSolve <- function(x, ...)
	{ 
	## Retrieve the Matrix Inverse if available
	m <- x$getinverse()
	if (!is.null(m))
		{
		## Cached Inverse already present so return 
		message("This Data is Cached!!!!!")
		return(m)
		}
	else
		{
		## Inverse not available so cache it for later  
		message("This Data is NOT Cached!!!!")
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		return(m)
		}
	} 
