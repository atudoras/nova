test_that("null_coalesce returns lhs when not NULL", {
  expect_equal(null_coalesce(5, 10), 5)
  expect_equal(null_coalesce("a", "b"), "a")
  expect_equal(null_coalesce(FALSE, TRUE), FALSE)
})

test_that("null_coalesce returns rhs when lhs is NULL", {
  expect_equal(null_coalesce(NULL, 10), 10)
  expect_equal(null_coalesce(NULL, "b"), "b")
})

test_that("null_coalesce treats NA as non-NULL (NA is not NULL)", {
  expect_equal(null_coalesce(NA, 10), NA)
  expect_equal(null_coalesce(NA_character_, "b"), NA_character_)
})

test_that("null_coalesce returns NULL when both lhs and rhs are NULL", {
  expect_null(null_coalesce(NULL, NULL))
})
