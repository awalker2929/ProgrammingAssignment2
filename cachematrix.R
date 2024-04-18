## Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function() {
  cache <- NULL
  
  set <- function(matrix) {
    inverse_matrix <- solve(matrix)
    cache <<- list(matrix = matrix, inverse = inverse_matrix)
  }
  
  get <- function() {
    cache$inverse
  }
  
  list(set = set, get = get)
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(cache_matrix) {
  if(!is.null(cache_matrix$get())) {
    message("Getting cached inverse matrix...")
    return(cache_matrix$get())
  } else {
    message("Calculating inverse matrix...")
    inverse_matrix <- solve(cache_matrix$get("matrix"))
    cache_matrix$set(inverse_matrix)
    return(inverse_matrix)
  }
}
