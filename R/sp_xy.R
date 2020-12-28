#' sp_xy Ganglion cell spacing function.
#'
#' This function returns the spacing between retinal ganglion cell receptive fields. The x,y
#' coordinates are specified in degrees of visual angle. The reference point 0,0 is the center
#' of the visual field; the center of the fovea centralis.
#'
#' @param x
#' @param y
#' @param spacing.scalar
#'
#' @return
#' @export
#'
#' @examples
sp_xy <- function(x, y, spacing.scalar = 1) {
  e_n <- 1.63 * spacing.scalar
  e_s <- 1.13 * spacing.scalar
  e_t <- 1.67 * spacing.scalar
  e_i <- 1.49 * spacing.scalar

  s0 <- 1/120

  if(is.na(x) | is.na(y)) {return(NA)}

  if (x >= 0 & y <= 0) {
    return.val <- s0 * (sqrt(x^2 / e_t^2 + y^2 / e_s^2) + 1)
  } else if(x < 0 & y <= 0){
    return.val <- s0 * (sqrt(x^2 / e_n^2 + y^2 / e_s^2) + 1)
  } else if(x > 0 & y > 0) {
    return.val <- s0 * (sqrt(x^2 / e_t^2 + y^2 / e_i^2) + 1)
  } else if(x <= 0 & y >= 0) {
    return.val <- s0 * (sqrt(x^2 / e_n^2 + y^2 / e_i^2) + 1)
  } else {
    return.val <- NA
  }
  return(return.val)
}
