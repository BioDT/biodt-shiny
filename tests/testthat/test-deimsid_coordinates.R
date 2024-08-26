source("app/logic/deimsid_coordinates.R")

test_that("get_coords_deimsid() results in error when called with wrong argument type", {
  expect_error(get_coords_deimsid(TRUE), "input must be a character vector")
  expect_error(get_coords_deimsid(1234), "input must be a character vector")
})

test_that("get_coords_deimsid() results in NA when called with wrong string", {
  expect_equal(get_coords_deimsid("foo-bar-baz"), NA)
})
