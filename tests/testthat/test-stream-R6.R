test_that("R6 stream works", {

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


  expect_error(
    TokenStream$new(1:3),
    "named"
  )

  stream <- TokenStream$new(c(a=1, b=2))
  expect_error(
    stream$reset(100) ,
    "out of range"
  )


  stream <- TokenStream$new(c(a=1, b=2))
  expect_true(stream$check_name_seq('a'))
  expect_true(stream$check_name_seq(c('a', 'b')))
  expect_false(stream$check_name_seq('b'))

  expect_error(stream$assert_name_seq(c('b', 'a')))
  expect_error(stream$assert_name('b'))
  expect_true(stream$check_name('a'))

  expect_true(stream$check_value_seq(c(1, 2)))
  expect_error(stream$assert_value_seq(c(2, 2)))
  expect_true(stream$check_value(c(1)))
  expect_error(stream$assert_value(c(2)))

  expect_false(stream$end_of_stream())
  stream$consume(2)
  expect_true(stream$end_of_stream())


  stream <- TokenStream$new(c(a=1, b=2))
  stream$read_while(name = c('a', 'b'))

  expect_error(stream$read_until())
  expect_error(stream$read_while())


  stream <- TokenStream$new(c(a=1, b=2, c = 3))
  expect_equal(
    stream$read_until(name = 'c', inclusive = TRUE),
    c(a=1, b=2, c=3)
  )

  stream <- TokenStream$new(c(a=1, b=2, c = 3))
  expect_equal(
    stream$read_until(name = 'c', inclusive = FALSE),
    c(a=1, b=2)
  )


  stream <- TokenStream$new(c(a=1, b=2, c = 3))
  expect_equal(
    stream$read_until(name = 'd', inclusive = FALSE),
    c(a=1, b=2, c = 3)
  )

  stream <- TokenStream$new(c(a=1, b=2, c = 3))
  expect_length(
    stream$read_until(name = 'a', inclusive = FALSE),
    0
  )


})
