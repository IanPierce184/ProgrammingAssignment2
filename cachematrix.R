## These functions mimic object-oriented techniques using a functional 
## programming approach for computing and caching the inverse of a matrix. The 
## result is a list object in R whose list elements act in a similar manner to 
## object methods as commonly seen in Python programming, including a similar 
## syntax--instead of something like `my_matrix.get_inverse()` we instead use 
## `my_matrix$get_inverse()` because we are accessing list elements using `$` 
## in R.

## The first function acts as an "factory" that instantiates an environment for 
## its execution; this environment persists after execution and acts as the
## "object" containing relevant data and functions. The functions it contains 
## (properly called closures) hide their environments from the outside world; 
## the only way to access or change the data is through calling these functions 
## themselves.

## The second function is formulated to interact with the closures in
## the first function--specifially it uses the get_inverse(), change_inverse(),
## and get_matrix() functions found in the makeCacheMatrix() function for use 
## in computing and caching the inverse matrix.

## The makeCacheMatrix() function takes an invertible matrix as an argument and 
## returns a list object containing: the matrix and four functions that can be 
## used to get the current matrix, change the matrix to another matrix and
## reset the inverse to NULL, get the inverse of the current matrix if it has 
## already been computed and stored, and change the stored inverse matrix.

makeCacheMatrix <- function(x = matrix()) { # Assumes x is invertible!
  # set default inverse matrix to NULL
  inv_matrix <- NULL
  # use this function to change matrix values and reset inverse
  change_matrix <- function(new_matrix){ 
    x <<- new_matrix # assigns new value
    inv_matrix <<- NULL # resets inverse matrix to avoid storing "out of
    # date" data
  }
  get_matrix <- function(){ x } # gets current matrix
  # use this function to cache inverse matrix
  change_inverse <- function(new_inverse) { inv_matrix <<- new_inverse}
  get_inverse <- function(){ inv_matrix } # gets current inverse
  list(get_matrix = get_matrix,
       change_matrix = change_matrix,
       get_inverse = get_inverse,
       change_inverse = change_inverse)
  # returns list object containing: matrix, matrix inverse (or NULL), and
  # functions for changing and getting the matrix, and caching and getting
  # the inverse
}


## The cacheSolve() function computes the inverse matrix for matrices that have 
## been converted to list objects of the form returned by the makeCacheMatrix()
## function above. It checks whether the matrix inverse has already been
## computed and cached; if so it simply returns the cached inverse. If not,
## it computes the inverse of the current matrix and caches and returns this
## inverse.

cacheSolve <- function(x, ...) {
  # get current inverse and return current inverse if not NULL
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)){
    message("From cached inverse:")
    return(inv_matrix)
  }
  # if current inverse is NULL, get current matrix and compute inverse
  current_matrix <- x$get_matrix()
  inv_matrix <- solve(current_matrix)
  # set current inverse to computed inverse and return
  x$change_inverse(inv_matrix)
  inv_matrix
}

## Testing and demonstration

test_mat <- rbind(c(1,1),c(-1,1)) # 2X2 invertible test matrix
test_cache_mat <- makeCacheMatrix(test_mat) # constructs enhanced matrix object
test_cache_mat$get_matrix() # should return original 2X2 test_mat
test_cache_mat$get_inverse() # should be NULL
cacheSolve(test_cache_mat) # apply second function to compute inverse
test_cache_mat$get_inverse() # should no longer be NULL, gives inverse matrix
cacheSolve(test_cache_mat) # should return cached inverse wtih message
test_mat_2 <- 2*test_mat # new 2X2 invertible test matrix
test_cache_mat$change_matrix(test_mat_2) # change matrix
test_cache_mat$get_matrix() # should be the new test matrix
test_cache_mat$get_inverse() # should be NULL again
cacheSolve(test_cache_mat) # should compute the inverse for the new matrix