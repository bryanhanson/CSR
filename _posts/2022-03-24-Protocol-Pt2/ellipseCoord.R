ellipseCoord <- function(data, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200) {

  stopifnot(length(data) != 0)
  stopifnot(pcx != pcy)

  if (!is.data.frame(data) & !tibble::is_tibble(data)) {
    stop("Data must be of class data.frame, tbl_df, or tbl")
  }

  if (pcx == 0 | pcy == 0) {
    stop("No component is provided either in pcx or pcy, or both.")
  }

  if(conf.limit < 0 | conf.limit > 1) {
    stop("Confidence level should be between 0 and 1")
  }

  # matrix of data
  X <- as.matrix(data)

  # Sample size
  n <- nrow(X)

  # Confidence limit
  alpha <- as.numeric(conf.limit)

  # Number of points
  m <- as.numeric(pts)
  p <- seq(0, 2*pi, length = m)

  # # Hotelling’s T-square limit
  Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))

  # Coordinate points
  rx <- sqrt(Tsq_limit*stats::var(X[, pcx]))
  ry <- sqrt(Tsq_limit*stats::var(X[, pcy]))

  res.coord <- tibble::tibble(
    x = rx*cos(p) + mean(X[, pcx], na.rm = TRUE),
    y = ry*sin(p) + mean(X[, pcy], na.rm = TRUE)
    )

  return(res.coord)

}
