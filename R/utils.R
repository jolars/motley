
# Spread points across a disk ---------------------------------------------

spread_points <- function(x = 0, y = 0, r = 1, n = 250) {
  golden_angle <- pi * (3L - sqrt(5))
  theta <- (1L:n) * golden_angle
  rad   <- sqrt(1L:n) / sqrt(n)
  px    <- rad * cos(theta)
  py    <- rad * sin(theta)
  px <- px * (r / max(rad)) + x
  py <- py * (r / max(rad)) + y
  cbind(x = px, y = py)
}
