#' Takes a raster and returns binary representation
#' 
#' This simple function takes an in put raster and it returns an identical one
#'  that has been dichotomized (e.g., values are 0/1).  
#' @param x Input raster.
#' @param hi.val Value to assign to non-NA values (default = 1.0)
#' @param lo.val Value to assign to NA values (default = 1.0)
#' @return A copy of the raster (including projection) with NA values set to \code{lo.val} 
#'  and non-NA values set to \code{hi.val}.
#' @author Rodney J. Dyer <rjdyer@vcu.edu>
#' @export
#' @examples 
#' library(raster)
#' x <- matrix( rnorm(25), nrow=5)
#' x[1,4] <- x[2,4] <- NA
#' r <- raster( x )
#' d <- dichotomize(r)
#' d
#' values(d)
dichotomize <- function( x, hi.val=1.0, low.val=0.0 ) {
  if( !is( x, "RasterLayer"))
    stop("This function needs to have (at least) a raster passed to it.")
  ret <- x
  y <- values(ret)
  y[ !is.na(y) ] <- hi.val
  y[ is.na(y) ] <- low.val
  values( ret ) <- y
  return( ret )
}

