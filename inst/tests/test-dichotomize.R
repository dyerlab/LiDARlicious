context( "dichotomize.R")


test_that("checking",{

library(raster)
  
  expect_that( dichotomize(), throws_error() )
  expect_that( dichotomize("bob"), throws_error() )

  x <- matrix( rnorm(25), nrow=5)
  x[1,4] <- x[2,4] <- NA
  r <- raster( x )
  
  d <- dichotomize(r)
  expect_that( d, is_a("RasterLayer") )
  expect_that( nrow(d), is_equivalent_to(5))
  expect_that( ncol(d), is_equivalent_to(5))
  expect_that( values(d)[1], is_equivalent_to(1))
  expect_that( values(d)[4], is_equivalent_to(0))
  
  d <- dichotomize(r, low.val=-1, hi.val=27)
  expect_that( d[1], is_equivalent_to(27) )
  expect_that( d[4], is_equivalent_to(-1))
  expect_that( d[1] < 0, is_false() )
  expect_that( d[4] < 0, is_true() )
  expect_that( d[4], is_less_than(0) )
  expect_that( d[1], is_more_than(26) )
}
)