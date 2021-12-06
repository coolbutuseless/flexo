

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An environment object encapsulating a stream of tokens and functions for manipulating/interrogating these token.
#'
#' This is very similar to the R6 Class \code{TokenStream}, but it has no
#' dependencies
#'
#' @param named_values named vector containing the tokens. Usually the output
#'        from \code{lex()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stream <- function(named_values) {

  nn <- names(named_values)
  if (is.null(nn) || anyNA(nn) || any(nn == '')) {
    stop("All tokens must be named")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # @field named_values the original tokens
  # @field position current stream position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream <- new.env()
  stream$named_values <- named_values
  stream$position     <- 1L


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reset stream to the given absolute position.
  #
  # @param position absolute position in stream. Default: 1 i.e. the start
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$reset = function(position = 1L) {
    position <- as.integer(position)
    if (position < 1L | position > length(stream$named_values)) {
      stop("stream$reset(): position length out of range")
    }

    stream$position <- position

    invisible(stream)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Throw an error if a read is not within range
  #
  # @param start,n start position and number of values to read
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$assert_within_range = function(start, n) {
    if (!stream$check_within_range(start, n)) {
      stop("assert_within_range() failed.  Length: ", length(stream$named_values),
           " pos:", stream$position, " start:", start, " n:", n)
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check if a read is not within range
  #
  # @param start,n start position and number of values to read
  #
  # @return logical TRUE if values are within range of data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$check_within_range = function(start, n) {
    start > 0 && (start + n - 1L) <= length(stream$named_values)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the next names match the name sequence specified
  #
  # @param name_seq Expected sequence of names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$check_name_seq = function(name_seq) {
    actual_names <- stream$read_names(length(name_seq))
    res <- all.equal(actual_names, name_seq)
    isTRUE(res)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assert the next names match the name sequence specified
  #
  # @param name_seq Expected sequence of names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$assert_name_seq = function(name_seq) {
    stopifnot(stream$check_name_seq(name_seq))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the next name is one of the valid names specified
  #
  # @param valid_names Valid names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$check_name = function(valid_names) {
    actual_name <- stream$read_names(1)
    actual_name %in% valid_names
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assert the next name is one of the valid names specified
  #
  # @param valid_names Valid names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$assert_name = function(valid_names) {
    if (!stream$check_name(valid_names)) {
      stop("Next name '", names(stream$read(1)), "' is not in ", deparse(valid_names))
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the next values match the value sequence specified
  #
  # @param value_seq Expected sequence of values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$check_value_seq = function(value_seq) {
    actual_values <- stream$read_values(length(value_seq))
    res <- all.equal(actual_values, value_seq)
    isTRUE(res)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assert the next values match the value sequence specified
  #
  # @param value_seq Expected sequence of values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$assert_value_seq = function(value_seq) {
    stopifnot(stream$check_value_seq(value_seq))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check the next value is one of the valid values specified
  #
  # @param valid_values Valid values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$check_value = function(valid_values) {
    actual_value <- stream$read_values(1)
    actual_value %in% valid_values
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assert the next value is one of the valid values specified
  #
  # @param valid_values Valid values
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$assert_value = function(valid_values) {
    if (!stream$check_value(valid_values)) {
      stop("Next value '", stream$read(1), "' is not in ", deparse(valid_values))
    }
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Advance the stream
  #
  # @param n number of tokens by which to advance the stream. May be
  #        negative. New position must be within range of the data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$advance = function(n) {
    n <- as.integer(n)
    stream$assert_within_range(stream$position, n)

    stream$position <- stream$position + n

    invisible(stream)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read n named values from the given position
  #
  # @param n number of values to read
  # @param offset offset from given position
  #
  # @return named values at this position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$read = function(n, offset = 0) {
    if (n < 1) {
      return(NULL)
    }
    stream$assert_within_range(stream$position + offset, n)
    stream$named_values[stream$position + seq(0, n-1) + offset]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read n names from the given position
  #
  # @param n number of values to read
  # @param offset offset from given position
  #
  # @return names at this position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$read_names = function(n, offset = 0) {
    names(stream$read(n))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read n values from the given position
  #
  # @param n number of values to read
  # @param offset offset from given position
  #
  # @return values at this position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$read_values = function(n, offset = 0) {
    unname(stream$read(n))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume n tokens from the given position i.e. read and advance the stream
  #
  # @param n number of values to read
  #
  # @return values starting at this position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$consume = function(n) {
    res <- stream$read(n, offset = 0)
    stream$advance(n)
    res
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # @description has end of stream been reached?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$end_of_stream = function() {
    stream$position > length(stream$named_values)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume tokens while some expression matches
  #
  # @param name,value the boundary of the consumption. if both name and
  #        value are specified, then \code{combine} indicates how to logically
  #        define the combination
  # @param combine logical operator value values: and, or
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$read_while = function(name = NULL, value = NULL, combine = 'or') {
    if (is.null(name) && is.null(value)) {
      stop("Must define either name or value")
    }

    search_idx <- seq(stream$position, length(stream$named_values))
    if (is.null(name)) {
      nidx <- FALSE
    } else {
      nidx <- names(stream$named_values[search_idx])  %in%  name
    }

    if (is.null(value)) {
      vidx <- FALSE
    } else {
      vidx <- stream$named_values[search_idx]   %in% value
    }

    idx <- switch (
      combine,
      and = nidx & vidx,
      or  = nidx | vidx,
      stop("No such 'combine' method: ", combine)
    )

    if (!isTRUE(idx[1])) {
      # first value doesn't match which means there's no values to read!
      n <- 0L
    } else {
      # What is the lengh of the initial run of "TRUE" values?
      n <- rle(idx)$lengths[1]
    }

    stream$read(n)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume tokens while some expression matches
  #
  # @param name,value the boundary of the consumption. if both name and
  #        value are specified, then \code{combine} indicates how to logically
  #        define the combination
  # @param combine logical operator value values: and, or
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$consume_while = function(name = NULL, value = NULL, combine = 'or') {
    res <- stream$read_while(name = name, value = value, combine = combine)
    stream$advance(length(res))
    res
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read until some expression matches
  #
  # @param name,value the boundary of the consumption. if both name and
  #        value are specified, then \code{combine} indicates how to logically
  #        define the combination
  # @param combine logical operator value values: and, or
  # @param inclusive should the end-point be included in the returned results?
  #        Default: TRUE.  If FALSE, then the end-point is not returned, and
  #        the stream position is set to *before* this end-point
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$read_until = function(name = NULL, value = NULL, combine = 'or',
                        inclusive = TRUE) {
    if (is.null(name) && is.null(value)) {
      stop("Must define either name or value")
    }

    search_idx <- seq(stream$position, length(stream$named_values))
    if (is.null(name)) {
      nidx <- FALSE
    } else {
      nidx <- names(stream$named_values[search_idx])  %in%  name
    }

    if (is.null(value)) {
      vidx <- FALSE
    } else {
      vidx <- stream$named_values[search_idx]   %in% value
    }

    idx <- switch (
      combine,
      and = nidx & vidx,
      or  = nidx | vidx,
      stop("No such 'combine' method: ", combine)
    )

    if (isTRUE(idx[1])) {
      # First item matches!
      if (inclusive) {
        n <- 1L
      } else {
        n <- 0L
      }
    } else if (!any(idx)) {
      # No match found. Read until end
      n <- length(idx)
    } else {
      n <- rle(idx)$length[1]
      if (inclusive) {
        n <- n + 1L
      }
    }


    stream$read(n)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Consume until some expression matches
  #
  # @param name,value the boundary of the consumption. if both name and
  #        value are specified, then \code{combine} indicates how to logically
  #        define the combination
  # @param combine logical operator value values: and, or
  # @param inclusive should the end-point be included in the returned results?
  #        Default: TRUE.  If FALSE, then the end-point is not returned, and
  #        the stream position is set to *before* this end-point
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$consume_until = function(name = NULL, value = NULL, combine = 'or', inclusive = TRUE) {
    res <- stream$read_until(name = name, value = value, combine = combine,
                             inclusive = inclusive)
    stream$advance(length(res))
    res
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Print current state
  #
  # @param n number of elements to print
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stream$print = function(n = 5) { # nocov start
    if (stream$end_of_stream()) {
      print("End of stream")
    } else {
      cat("Position ", stream$position, "/", length(stream$named_values), ".\n", sep = "")

      n2 <- length(stream$named_values) - stream$position + 1L
      n  <- min(n2, n)

      if (n > 0) {
        cat("Next", n, "elements:\n")
        print(stream$named_values[stream$position + seq(n) - 1L])
      }
    }
  } # nocov end


  stream
}


if (FALSE) {

  named_values <- c(one = 1, two = 2, three = 3, four = 4, five = 5)

  stream <- create_stream(named_values)

  stream$consume_until(name = 'one', inclusive = FALSE)
  stream$print()


  stream$consume_until(name = 'two', inclusive = FALSE)
  stream$print()
}



