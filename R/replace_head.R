#' @title replace_head
#'
#' @description Replace initial `n` values in a vector.
#'
#' @details A simple solution for replacing the initial `n` values in a vector.
#'   Replacing the initial `n` values is straightforward compared to replacing
#'   the final `n` values, but this makes a nice partner function to
#'   [replace_head()].
#'
#' @usage replace_head(v, n, r)
#'
#' @param v vector. Input vector to have head replaced.
#' @param n integer. Number of values at start of `v` to replace.
#' @param r String, logical, numeric etc. What to use as replacement. Should
#'   either be a single value or vector of length `n`.
#'
#' @return The `v` vector is returned but with the initial `n` values overwritten
#'   by `r.
#'
#' @examples
#' # Replace initial 5 numeric values with single value
#' x <- 1:10
#' replace_head(x, 5, 100)
#'
#' # Replace initial logical with a different logical
#' x <- rep(TRUE, 10)
#' replace_head(x, 1, FALSE)
#'
#' # Replace initial n with a vector of same length
#' x <- 1:20
#' replace_head(x, 5, c(100,99,98,97,96))
#'
#' @seealso replace_head
#' @author Nicholas Carey - \email{nicholascarey@gmail.com}
#' @md
#' @export

replace_head <- function(v, n, r){
  if(n > length(v) || n %% 1 != 0 || length(n) > 1)
    stop("replace_head: 'n' should be a single integer not greater than the length of 'v'.")
  if(length(r) != 1 && length(r) != n )
    stop("replace_head: 'r' input should be length 1 or length 'n'.")
  v_out <- v
  v_out[1:n] <- r
  return(v_out)
}


