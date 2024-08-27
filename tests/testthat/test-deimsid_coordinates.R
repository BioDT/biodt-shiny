box::use(
  testthat[expect_equal, expect_error, test_that],
)

# need to source a function, which we want to test
source("../../app/logic/deimsid_coordinates.R")

test_that("get_coords_deimsid() results in error when called with wrong argument type", {
  expect_error(get_coords_deimsid(TRUE), "input must be a character vector")
  expect_error(get_coords_deimsid(1234), "input must be a character vector")
})

test_that("get_coords_deimsid() results in NA when called with wrong DEIMS.ID", {
  expect_equal(get_coords_deimsid("foo-bar-baz"), NA)
})

test_that("if valid DEIMS.ID (eg. like '5b409a72-2a45-4238-a501-e24f1a2900db') is provided, the function returns longitude and latitude of the DEIMS site", {
  expect_equal(typeof(get_coords_deimsid("5b409a72-2a45-4238-a501-e24f1a2900db")$lng), "double")
  expect_equal(typeof(get_coords_deimsid("5b409a72-2a45-4238-a501-e24f1a2900db")$lat), "double")
})