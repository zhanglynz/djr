test_that("multiplication works", {
  my_df <- data.frame(id = letters[1:5],
                      x = c(1:3, 4.1, NA),
                      y = LETTERS[1:5],
                      z = c(NA, NA, 1:3),
                      w = rep(NA, 5),
                      u = rep("", 5))
  new_df <- get_col_sums_added(a_df = my_df)
  df_2 <- data.frame(id = c(letters[1:5], "Total"),
                     x = c(1:3, 4.1, NA, sum(c(1:3, 4.1))),
                     y = c(LETTERS[1:5], NA_real_),
                     z = c(NA, NA, 1:3, sum(c(1:3, NA_real_), na.rm = TRUE)),
                     w = c(rep(NA, 5), NA_real_),
                     u = c(rep("", 5), NA_real_))
  test <- identical(new_df, df_2)
  expect_equal(test, TRUE)
})
