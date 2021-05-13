#' fit_maps interpolates a 1d d' efficiency map and produces a 2d map that is used for search.
#'
#' @param maps_1d
#'
#' @return
#' @export
#'
#' @examples
fit_maps <- function(maps_1d) {
  dist.mat   <- expand.grid(X = (-1199:1200) / 120, Y = (-1199:1200) / 120)
  dist.mat   <- dist.mat %>%
    mutate(cdeg = sqrt(X^2 + Y^2))

  # Perform the interpolation on spacing in parallel for speed
  n_row <- length(maps_1d)

  interpolated_maps <- lapply(1:n_row, FUN = function(x) {
    interpolate_single_map(maps_1d[[x]]$c_deg, maps_1d[[x]]$y.scale, dist.mat$cdeg)
  })

  return(interpolated_maps)
}

#' Single interpolated map
#'
#' @param eccentricity
#' @param eccentricity_full
#' @param scalar
#'
#' @return
#' @export
#'
#' @examples
interpolate_single_map <- function(eccentricity, scalar, eccentricity_full) {
    eccentricity_full_vec <- as.numeric(eccentricity_full)
    scale.interp <- Hmisc::approxExtrap(eccentricity, scalar, eccentricity_full_vec)$y

    scale.interp <- replace(scale.interp, scale.interp < 0, 0)
    scale.interp <- replace(scale.interp, scale.interp > 1, 1)

    scale.interp <- matrix(scale.interp, ncol = 2400, nrow = 2400)
    return(scale.interp)
}
