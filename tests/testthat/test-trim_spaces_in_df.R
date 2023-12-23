test_that("trim_spaces_in_df", {
  df <- data.frame(x = 1:5,
                   y = c('a b ', ' cd e ', "", '  fgh   i  ', 'ok'),
                   z = rep(TRUE, 5))
  new_df <- trim_spaces_in_df(df)
  df_2 <- data.frame(x = 1:5,
                     y = c('a b', 'cd e', "", 'fgh i', 'ok'),
                     z = rep(TRUE, 5))
  test <- identical(new_df, df_2)
  expect_equal(test, TRUE)
})
