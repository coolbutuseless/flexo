


test_that("consume_until works", {


  named_values <- c(one = 1, two = 2, three = 3, four = 4, five = 5)
  stream <- TokenStream$new(named_values)

  jnk <- stream$consume_until(name = 'one', inclusive = FALSE)
  expect_equivalent(stream$read(1), 1)

  jnk <- stream$consume_until(name = 'one', inclusive = FALSE)
  expect_equivalent(stream$read(1), 1)

  jnk <- stream$consume_until(name = 'two', inclusive = FALSE)
  expect_equivalent(stream$read(1), 2)

  jnk <- stream$consume_until(name = 'two', inclusive = FALSE)
  expect_equivalent(stream$read(1), 2)





  stream <- TokenStream$new(named_values)

  jnk <- stream$consume_until(name = 'one', inclusive = TRUE)
  expect_equivalent(stream$read(1), 2)

  jnk <- stream$consume_until(name = 'one', inclusive = FALSE)
  expect_true(stream$end_of_stream())

  stream$reset()

  jnk <- stream$consume_until(name = 'two', inclusive = TRUE)
  expect_equivalent(stream$read(1), 3)

  jnk <- stream$consume_until(name = 'two', inclusive = TRUE)
  expect_true(stream$end_of_stream())


})
