

# Example taken from Issue #2 on github
test_that("consume while logic fix", {

  sentence_regexes <- c(
    word        = "\\w+",
    whitespace  = "\\s+",
    fullstop    = "\\.",
    comma       = ","
  )

  sentence <- "Hello there, Rstats."
  tokens <- lex(sentence, sentence_regexes)
  tokens

  s1 <- TokenStream$new(tokens)
  res <- s1$read_while("whitespace")
  res

  expect_length(res, 0)
})


test_that("read zero values works", {

  sentence_regexes <- c(
    word        = "\\w+",
    whitespace  = "\\s+",
    fullstop    = "\\.",
    comma       = ","
  )

  sentence <- "Hello there, Rstats."
  tokens <- lex(sentence, sentence_regexes)
  tokens

  s1 <- TokenStream$new(tokens)

  expect_length(s1$read(4), 4)
  expect_length(s1$read(3), 3)
  expect_length(s1$read(2), 2)
  expect_length(s1$read(1), 1)
  expect_length(s1$read(0), 0)

})
