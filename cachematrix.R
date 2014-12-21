#' Takes a numeric matrix and produces a cache object with accessors
#' for efficiently computing and accessing the supplied matrix's
#' inverse.  Once computed, the inverse is cached and returned without
#' additional computation unless the returned cache is initialized
#' with a new matrix.
#'
#' @param x Numeric matrix whose inverse will be computed.  The initial value may later be overriden via the set() method.
#'
#' @return A cache object with the following methods: set(), get(), set_inverse(), get_inverse().  These allow a new matrix to be set, the current matrix to be acquired, a new cached inverse to be stored, and the existing cached inverse to be returned.
makeCacheMatrix <- function( x = numeric() )
{
    # we start out with an empty cache without a computed inverse.
    # this will be initialized to empty via set() and accessed via
    # set_inverse() and get_inverse().
    inverse <- NULL

    # takes a matrix and stores it into this function's environment for
    # future access.  the existing inverse is reset to ensure that it is
    # lazily recomputed when it is needed next.
    set <- function( y )
    {
        x       <<- y
        inverse <<- NULL
    }

    # gets the current matrix from this function's environment.  the value
    # returned is the same as the last one set().
    get <- function( )
    {
        x
    }

    # takes a matrix and stores it in this function's environment as the
    # inverse to the previously specified matrix via set().
    set_inverse <- function( inv )
    {
        # XXX: we should probably check that inv is actually the inverse to x
        #      before caching it.
        inverse <<- inv
    }

    # gets the current matrix inverse from this function's environment.
    #
    # NOTE: this may be NULL if set_inverse() has not been called since the
    #       last call to set().
    get_inverse <- function( )
    {
        inverse
    }

    # our cache "object" is a named list of the functions defined above, each of
    # which reference variables set in this function's environment.
    list( set = set,
          get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse )
}

#' Takes a cache object, as returned from makeCacheMatrix(), and returns the cached
#' inverse matrix associated with it.  Care is taken to return a cached inverse
#' without additional computation if it is available, otherwise computes the inverse
#' and stores it in the object for future access.
#'
#' @param x List returned from makeCacheMatrix() to query for a cached inverse.  If the supplied list does not have an inverse cached, it is computed and cached for future invocations.
#' @param ... Optional arguments that are passed to the solve() function when computing the inverse of x.
#'
#' @return Returns the inverse of x.
cacheSolve <- function( x, ... )
{
    # query the cache object for an existing inverse.
    inverse <- x$get_inverse()

    # is the cache empty?
    if( !is.null( inverse ) )
    {
        # we have a cache hit and can return without doing further work.
        message( "Getting cached inverse." )
        return( inverse )
    }

    # the cache is empty, get the matrix to invert.
    data <- x$get()

    # compute the inverse of the stored matrix.
    inverse <- solve( data, ... )

    # cache the inverse so that future executions will reuse our work.
    x$set_inverse( inverse )

    inverse
}
