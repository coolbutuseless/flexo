

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An R6 class for manipulating/interrogating a stream of tokens.
#'
#' @import R6
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TokenStream <- R6::R6Class(
  "TokenStream",
  public = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field named_values the original tokens
    #' @field position current stream position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    named_values  = NULL,
    position      = 1L,


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Initialise a stream
    #'
    #' @param named_values named vector of values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(named_values) {

      nn <- names(named_values)
      if (is.null(nn) || anyNA(nn) || any(nn == '')) {
        stop("All tokens must be named")
      }


      self$named_values <- named_values
      self$reset()
      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Reset stream to the given absolute position.
    #'
    #' @param position absolute position in stream. Default: 1 i.e. the start
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    reset = function(position = 1L) {
      position <- as.integer(position)
      if (position < 1L | position > length(self$named_values)) {
        stop("TokenStream$reset(): position length out of range")
      }

      self$position <- position

      invisible(self)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Throw an error if a read is not within range
    #'
    #' @param start,n start position and number of values to read
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_within_range = function(start, n) {
      if (!self$check_within_range(start, n)) {
        stop("assert_within_range() failed.  Length: ", length(self$named_values),
             " pos:", self$position, " start:", start, " n:", n)
      }
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Check if a read is not within range
    #'
    #' @param start,n start position and number of values to read
    #'
    #' @return logical TRUE if values are within range of data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_within_range = function(start, n) {
      start > 0 && (start + n - 1L) <= length(self$named_values)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Check the next names match the name sequence specified
    #'
    #' @param name_seq Expected sequence of names
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_name_seq = function(name_seq) {
      actual_names <- self$read_names(length(name_seq))
      res <- all.equal(actual_names, name_seq)
      isTRUE(res)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Assert the next names match the name sequence specified
    #'
    #' @param name_seq Expected sequence of names
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_name_seq = function(name_seq) {
      stopifnot(self$check_name_seq(name_seq))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Check the next name is one of the valid names specified
    #'
    #' @param valid_names Valid names
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_name = function(valid_names) {
      actual_name <- self$read_names(1)
      actual_name %in% valid_names
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Assert the next name is one of the valid names specified
    #'
    #' @param valid_names Valid names
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_name = function(valid_names) {
      if (!self$check_name(valid_names)) {
        stop("Next name '", names(self$read(1)), "' is not in ", deparse(valid_names))
      }
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Check the next values match the value sequence specified
    #'
    #' @param value_seq Expected sequence of values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_value_seq = function(value_seq) {
      actual_values <- self$read_values(length(value_seq))
      res <- all.equal(actual_values, value_seq)
      isTRUE(res)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Assert the next values match the value sequence specified
    #'
    #' @param value_seq Expected sequence of values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_value_seq = function(value_seq) {
      stopifnot(self$check_value_seq(value_seq))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Check the next value is one of the valid values specified
    #'
    #' @param valid_values Valid values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_value = function(valid_values) {
      actual_value <- self$read_values(1)
      actual_value %in% valid_values
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Assert the next value is one of the valid values specified
    #'
    #' @param valid_values Valid values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    assert_value = function(valid_values) {
      if (!self$check_value(valid_values)) {
        stop("Next value '", self$read(1), "' is not in ", deparse(valid_values))
      }
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Advance the stream
    #'
    #' @param n number of tokens by which to advance the stream. May be
    #'        negative. New position must be within range of the data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    advance = function(n) {
      n <- as.integer(n)
      self$assert_within_range(self$position, n)

      self$position <- self$position + n

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Read n named values from the given position
    #'
    #' Returns values but does not advance stream position
    #'
    #' @param n number of values to read
    #' @param offset offset from given position
    #'
    #' @return named values at this position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read = function(n, offset = 0) {
      if (n < 1) {
        return(NULL)
      }
      self$assert_within_range(self$position + offset, n)
      self$named_values[self$position + seq(0, n-1) + offset]
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Read n names from the given position
    #'
    #' Returns values but does not advance stream position
    #'
    #' @param n number of values to read
    #' @param offset offset from given position
    #'
    #' @return names at this position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_names = function(n, offset = 0) {
      names(self$read(n))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Read n values from the given position
    #'
    #' Returns values but does not advance stream position
    #'
    #' @param n number of values to read
    #' @param offset offset from given position
    #'
    #' @return values at this position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_values = function(n, offset = 0) {
      unname(self$read(n))
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Consume n tokens from the given position i.e. read and advance the stream
    #'
    #' Returns values and advances stream position.
    #'
    #' @param n number of values to read
    #'
    #' @return values starting at this position
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    consume = function(n) {
      res <- self$read(n, offset = 0)
      self$advance(n)
      res
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description has end of stream been reached?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    end_of_stream = function() {
      self$position > length(self$named_values)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Read tokens while some expression matches
    #'
    #' Returns values but does not advance stream position
    #'
    #' @param name,value the boundary of the consumption. if both name and
    #'        value are specified, then \code{combine} indicates how to logically
    #'        define the combination
    #' @param combine logical operator value values: and, or
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_while = function(name = NULL, value = NULL, combine = 'or') {
      if (is.null(name) && is.null(value)) {
        stop("Must define either name or value")
      }

      stopifnot(!self$end_of_stream())

      search_idx <- seq(self$position, length(self$named_values))
      if (is.null(name)) {
        nidx <- FALSE
      } else {
        nidx <- names(self$named_values[search_idx])  %in%  name
      }

      if (is.null(value)) {
        vidx <- FALSE
      } else {
        vidx <- self$named_values[search_idx]   %in% value
      }

      idx <- switch (
        combine,
        and = nidx & vidx,
        or  = nidx | vidx,
        stop("No such 'combine' method: ", combine)
      )

      if (!isTRUE(idx[1])) {
        # first value doesn't match which means there's no values to read!
        # Call self$read(0) so that the return value is always consistent
        n <- 0L
      } else {
        # What is the lengh of the initial run of "TRUE" values?
        n <- rle(idx)$lengths[1]
      }

      self$read(n)
    },

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Consume tokens while some expression matches
    #'
    #' Returns values and advances stream position.
    #'
    #' @param name,value the boundary of the consumption. if both name and
    #'        value are specified, then \code{combine} indicates how to logically
    #'        define the combination
    #' @param combine logical operator value values: and, or
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    consume_while = function(name = NULL, value = NULL, combine = 'or') {
      res <- self$read_while(name = name, value = value, combine = combine)
      self$advance(length(res))
      res
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Read until some expression matches
    #'
    #' Returns values but does not advance stream position
    #'
    #' @param name,value the boundary of the consumption. if both name and
    #'        value are specified, then \code{combine} indicates how to logically
    #'        define the combination
    #' @param combine logical operator value values: and, or
    #' @param inclusive should the end-point be included in the returned results?
    #'        Default: TRUE.  If FALSE, then the end-point is not returned, and
    #'        the stream position is set to *before* this end-point
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    read_until = function(name = NULL, value = NULL, combine = 'or',
                          inclusive = TRUE) {
      if (is.null(name) && is.null(value)) {
        stop("Must define either name or value")
      }

      search_idx <- seq(self$position, length(self$named_values))
      if (is.null(name)) {
        nidx <- FALSE
      } else {
        nidx <- names(self$named_values[search_idx])  %in%  name
      }

      if (is.null(value)) {
        vidx <- FALSE
      } else {
        vidx <- self$named_values[search_idx]   %in% value
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


      self$read(n)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Consume until some expression matches
    #'
    #' Returns values and advances stream position.
    #'
    #' @param name,value the boundary of the consumption. if both name and
    #'        value are specified, then \code{combine} indicates how to logically
    #'        define the combination
    #' @param combine logical operator value values: and, or
    #' @param inclusive should the end-point be included in the returned results?
    #'        Default: TRUE.  If FALSE, then the end-point is not returned, and
    #'        the stream position is set to *before* this end-point
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    consume_until = function(name = NULL, value = NULL, combine = 'or', inclusive = TRUE) {
      res <- self$read_until(name = name, value = value, combine = combine,
                             inclusive = inclusive)
      self$advance(length(res))
      res
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Print current state
    #'
    #' @param n number of elements to print
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    print = function(n = 5) { # nocov start
      if (self$end_of_stream()) {
        print("End of stream")
      } else {
        cat("Position ", self$position, "/", length(self$named_values), ".\n", sep = "")

        n2 <- length(self$named_values) - self$position + 1L
        n  <- min(n2, n)

        if (n > 0) {
          cat("Next", n, "elements:\n")
          print(self$named_values[self$position + seq(n) - 1L])
        }
      }
    } # nocov end

  )
)



if (FALSE) {

  named_values <- c(one = 1, two = 2, three = 3, four = 4, five = 5)

  stream <- TokenStream$new(named_values)

  stream$consume_until(name = 'one', inclusive = FALSE)
  stream


  stream$consume_until(name = 'two', inclusive = FALSE)
  stream
}



