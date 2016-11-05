#' Spread points across a disk using Vogel's method
#'
#' Spreads points evenly across a disk of any radius using the golden
#' angle. Defaults to sampling 250 points on the
#' \href{https://en.wikipedia.org/wiki/Unit_disk}{unit disk}.
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
#' @import assertthat
#' @export

sample_disk <- function(x = 0, y = 0, r = 1, n = 250) {
  assert_that(
    is.numeric(x),
    is.numeric(y),
    is.numeric(r),
    is.count(n),
    r > 0
  )
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
#' @seealso \code{\link[base]{expand.grid}}, \code{\link[utils]{combn}}
#'
#' @examples
#' combine_binaries(3)
#'
#' @import assertthat
#' @export

combine_binaries <- function(n) {
  assert_that(is.count(n))
  ll <- vector("list", n)
  for (i in seq_along(ll)) ll[[i]] <- 0L:1L
  as.matrix(expand.grid(ll, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
}

#' Normalize (scale) numeric vector or matrix to any range
#'
#' Take a numeric vector or matrix and scale it to a new range.
#'
#' @param x A numeric vector or matrix
#' @param new_min The new min value for the input
#' @param new_max The new max value for the input
#' @return A matrix or vector with values normalized to the given range.#'
#' @seealso \link[base]{scale}
#' @examples
#' normalize(1:10, new_min = -10, new_max = -9)#'
#' @import assertthat
#' @export
normalize <- function(x, new_min = 0, new_max = 1) {
  assert_that(
    is.numeric(x),
    is.scalar(new_min),
    is.scalar(new_max)
  )
  (new_max - new_min) / (max(x) - min(x)) * (x - max(x)) + new_max
}
