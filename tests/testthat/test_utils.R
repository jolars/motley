context("Utility functions")

test_that("erroneous inputs returns errors", {
  expect_error(sample_disk("2"))
  expect_error(sample_disk(f = 19))
  expect_error(sample_disk(r = 0))
  expect_error(sample_disk(n = 1.5))
  expect_error(sample_disk(n = 0))
  expect_error(sample_disk(n = -1))
  expect_error(sample_disk(r = -1))
})

test_that("correct input returns no errors", {
  expect_error(sample_disk(), NA)
  expect_error(sample_disk(x = -1000, y = 984, r = 0.2, n = 1), NA)
})
