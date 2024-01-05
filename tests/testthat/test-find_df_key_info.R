test_that("multiplication works", {
  temp_df <-
    data.frame(a = 1:10,
               b = NA,
               e = c(letters[1:8], NA, NA),
               stringsAsFactors = FALSE)
  info <- find_df_key_info(temp_df)
  names(info$sample_values) <- NULL
  info_df <-
    data.frame(var_name = c('a', 'b', 'e'),
               var_type = c('integer', 'logical', 'character'),
               var_class = c('integer', 'logical', 'character'),
               nbr_of_rows = rep(10L, 3),
               nbr_of_unique_rows = c(10, 1.0, 9),
               nbr_of_NAs = c(0.0, 10, 2),
               the_min = c(1.0, NA, NA),
               the_max = c(10.0, NA, NA),
               width_min = c(1, 0.0, 1),
               width_max = c(2, 0.0, 1),
               sample_values = I(list(1L:10L, NA, c(letters[1:8], NA))),
               stringsAsFactors = FALSE)
  test <- identical(info, info_df)
  expect_equal(test, TRUE)
})
