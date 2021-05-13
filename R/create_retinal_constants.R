#' create_retinal_constants The function used to generate the retinal parameters that determine the number of GCs on a ring.
#'
#' @param version
#'
#' @return
#' @export
#'
#' @examples
create_retinal_constants <- function(version = "standard") {
  if(version == "standard") {
    ring_centers <-  c(0, 0.387, 0.902, 1.594, 2.514, 3.726, 5.298, 7.304, 9.829)
    patch_radii   <- c(0.168, 0.219, 0.296, 0.396, 0.525, 0.687, 0.885, 1.121, 1.404)
    pix_per_patch <- c(1274, 2177, 3952, 7089, 12470, 21350, 35397, 56864, 89203)
    n_patches     <- c(1, 5.776, 9.956, 12.98, 15.1, 16.5, 17.36, 17.86, 18.15)


    parameters <- list(ring_centers = ring_centers,
                       patch_radii = patch_radii,
                       pix_per_patch = pix_per_patch,
                       n_patches = n_patches)
  } else if(version == "expanded_fovea") {
    ring_centers <- round(c(
      0, 0.459800349073796, 0.973111993685347, 1.56256206738979, 2.32554893517623, 3.38989375592665, 4.90757077825768, 7.04333171846555, 9.97431500944187
    ), 3)
    patch_radii <- c(0.211956913605676, 0.247843435468120, 0.265468209143432, 0.323981864561015, 0.439005003225417, 0.625339817525009, 0.892337204806020, 1.24342373540184, 1.68755955557449)
    pix_per_patch <- c(2032, 2779, 3188, 4748, 8719, 17691, 36022, 69944, 128834)
    n_patches <- c(1, 6.85159149, 10.7275778, 12.73282105, 13.99479252, 15.06258584, 16.13548668, 17.24893549, 18.40780099)

    parameters <- list(ring_centers = ring_centers,
                       patch_radii = patch_radii,
                       pix_per_patch = pix_per_patch,
                       n_patches = n_patches)

  } else {
    warning("no matching selection made")
  }

}
