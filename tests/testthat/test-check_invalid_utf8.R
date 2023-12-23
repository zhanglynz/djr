test_that("check_invalid_utf8", {
  x <- c("abc\xa0, def", "123")
  y <- any(check_invalid_utf8(x))
  expect_equal(y, TRUE)
  z <- convert_to_utf8(x)
  w <- all(!check_invalid_utf8(z))
  expect_equal(w, TRUE)
})
