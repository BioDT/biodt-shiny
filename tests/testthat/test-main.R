# box::use(
#   shiny[testServer],
#   testthat[expect_true, test_that],
# )
# box::use(
#   app/main[server],
#   app/main[ui]
# )

# test_that("main server works", {
#   testServer("app/main.R", {
#     print("output$message$html:::", output$message$html)
#     expect_true(grepl(x = output$message$html, pattern = "Check out Rhino docs!"))
#   })
# })
