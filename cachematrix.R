## There are 2 functions makeCacheMatrix & cacheSolve
## makeCacheMatrix is a helper function which cache the 
## inversion of a given square matrix. 
## cacheSolve function computes the invert for the given 
## cachedMatrix list object created by makeCacheMatrix function.

# makeCacheMatrix function takes a 'matrix' object as input and
# returns list of methods operated on the given matrix to cache
# the inverted matrix and retrieve it. It also ensures if the matrix 
# gets modified, inverted matrix is reset to null.

# INPUT <- A Square Matrix object
# OUTPUT <- A List Object holding functions to operate on the matrix and its invert
makeCacheMatrix <- function(x = matrix()) {
	
	# Initialize inverted matrix
	inv_m <- NULL
	set <- function(y) {
		x <<- y
        # When the matrix changes, ensuring inversion is reset
		inv_m <<- NULL
	}

    # Getter method to acquire the original matrix
	get <- function() x

    # set and get inverted matrix.
	set_inverse_matrix <- function(inv) inv_m <<- inv
	get_inverse_matrix <- function() inv_m

    # return the list of methods for inversion operation
	list(set = set, get = get, 
		set_inverse_matrix = set_inverse_matrix, 
		get_inverse_matrix = get_inverse_matrix)
}


# cacheSolve takes 'list' object created thru makeCacheMatrix function.
# It checks for the inverted matrix, if it is computed already it returns.
# If it is not computed, it computes and updates the cached matrix object.

# INPUT <- A List Object holding functions to operate on the matrix and its invert
# OUTPUT <- Inverted Matrix
cacheSolve <- function(x, ...) {

    # Get the inverted matrix
	inv_m <- x$get_inverse_matrix()

    # if it is already inverted, return the inverted one
	if(!is.null(inv_m)) {
		message("getting cached inverse matrix")
		return(inv_m)
	}
    
    # if it is not inverted, acquire the matrix and invert it
	data <- x$get()

    # Only a square matrix can be inverted. Ensure the given matrix is invertible
	inv_m <- solve(data, ...)

    # ensure the inverted matrix is cached
    x$set_inverse_matrix(inv_m)

    # return the inverted matrix
	inv_m

}
