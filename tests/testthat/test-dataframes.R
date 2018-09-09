context("Creating data frames with labelled vars")

test_that("data.frame variables are named correctly", {
  d <- data.frame(
    lab = labelled(1:5, c(a=1, b=2)),
    nolab = 1:5
  )
  expect_equal(names(d), c("lab", "nolab"))
})

test_that("tibble variables are named correctly", {
    d <- dplyr::tibble(
    lab = labelled(1:5, c(a=1, b=2)),
    nolab = 1:5
  )
  expect_equal(names(d), c("lab", "nolab"))
})
