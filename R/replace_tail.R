#' @title replace_tail
#'
#' @description Replace final `n` values in a vector.
#'
#' @details A simple solution to the lack of an elegant way in `R` of simply
#'   replacing the last `n` values in a vector. There are lots of ways of
#'   extracting the last `n` values, and a few of these can be used to replace
#'   them, but they are all not very elegant in code. There is a similar
#'   function for the (much easier) operation to replace the initial `n` values:
#'   [replace_head()].
#'
#' @usage replace_tail(v, n, r)
#'
#' @param v vector. Input vector to have tail replaced.
#' @param n integer. Number of values at tail of `v` to replace. If left NULL,
#'   this will be calculated as the length of `r`.
#' @param r String, logical, numeric etc. What to use as replacement. Should
#'   either be a single value or vector of length `n`.
#'
#' @return The `v` vector is returned but with the final `n` values overwritten
#'   by `r`.
#'
#' @examples
#' # Replace last 5 numeric values with single value
#' x <- 1:10
#' replace_tail(x, 5, 100)
#'
#' # Replace last logical with a different logical
#' x <- rep(TRUE, 10)
#' replace_tail(x, 1, FALSE)
#'
#' # Replace last n with a vector of same length
#' x <- 1:20
#' replace_tail(x, 5, c(100,99,98,97,96))
#'
#' @seealso replace_head
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#' @md
#' @export

replace_tail <- function(v = NULL, n = NULL, r = NULL){
  ## if n is NULL, just make it same length as r
  if(is.null(n)) n <- length(r)

  if(n > length(v) || n %% 1 != 0 || length(n) > 1)
    stop("replace_tail: 'n' should be a single integer not greater than the length of 'v'.")
  if(length(r) != 1 && length(r) != n )
    stop("replace_tail: 'r' input should be length 1 or length 'n'.")
  v_out <- v
  v_out[(length(v_out)-n+1):length(v_out)] <- r
  return(v_out)
}
