#' Spread points across a disk using Vogel's method
#'
#' Spreads points evenly across a disk of any radius by making use of the golden
#' angle. Defaults to
#' \href{https://en.wikipedia.org/wiki/Unit_disk}{the unit disk}.
#'
#' The function is vectorized.
#'
#' @param x X coordinate for the center of the disk.
#' @param y Y coordinate for the center of the disk.
#' @param r Radius.
#' @param n Number of points to generate.
#' @return A matrix of \code{x} and \code{y} coordinates.
#'
#' @examples
#' pp <- sample_disk(x = 10, y = -5, r = 5, n = 800)
#' head(pp)
#' plot(pp, asp = 1, pch = 19)
#' @importFrom assertthat assert_that
#' @export

sample_disk <- function(x = 0, y = 0, r = 1, n = 250) {
  assert_that(is.numeric(x))
  assert_that(is.numeric(y))
  assert_that(is.numeric(r))
  assert_that(is.numeric(n))
  assert_that(r > 0)
  assert_that(n > 0)
  assert_that(n %% 1 == 0)

  n <- n - 1

  theta <- 0L:n * pi * (3L - sqrt(5L))
  rad <- sqrt(0L:n) / sqrt(n)
  px <- rad * cos(theta)
  py <- rad * sin(theta)
  x <- px * r / max(rad) + x
  y <- py * r / max(rad) + y
  cbind(x, y)
}

#' Generate a matrix of binary combinations
#'
#' Generates a matrix of all possible combinations of binary values (0s and 1s).
#'
#' @param n The number of combinations to produce.
#' @return A matrix of binary combinations, one for each row.
#' @seealso \code{\link[base]{expand.grid}}
#'
#' @examples
#' combine_binaries(3)
#'
#' @importFrom assertthat assert_that
#' @export

combine_binaries <- function(n) {
  assert_that(n > 0)
  assert_that(n %% 1 == 0)
  assert_that(is.numeric(n))

  ll <- vector("list", n)
  for (i in seq_along(ll)) ll[[i]] <- 0L:1L
  as.matrix(expand.grid(ll, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
}
