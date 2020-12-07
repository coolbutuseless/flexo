test_that("stream works", {

  named_values <- c(one = 1, two = 2, three = 3, four = 4, five = 5)
  stream <- TokenStream$new(named_values)

  expect_identical(stream$read(3), named_values[1:3])

  expect_error(stream$read(6), "assert_within_range")


  res <- stream$consume(2)
  expect_identical(res, named_values[1:2])
  expect_equal(stream$position, 3)

  res <- stream$consume_until(value = 4)
  expect_identical(res, named_values[3:4])
  expect_equal(stream$position, 5)


  stream$reset()
  expect_equal(stream$position, 1)

  res <- stream$consume_while(value = 1:4)
  expect_identical(res, named_values[1:4])
  expect_equal(stream$position, 5)

})
