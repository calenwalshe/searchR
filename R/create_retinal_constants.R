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
    ring_centers  <- round(c(0, 0.387155159524209, 0.902088012775735, 1.59351713353973, 2.51439251312396, 3.72640073357580, 5.29793949271154, 7.30363750987555, 9.82899674331473), 3)
    patch_radii   <- c(0.167799223271160, 0.219355936253049, 0.295576916998478, 0.395852203765516, 0.525023175818718, 0.686985044633115, 0.884553714502628, 1.12114430266138, 1.40421493077780)
    pix_per_patch <- c(1274, 2177, 3952, 7089, 12470, 21350, 35397, 56864, 89203)
    g_cell_patch  <- c(1134, 1053, 1059, 1070, 1085, 1105, 1125, 1142, 1151)
    n_patches     <- c(1, 5.776, 9.956, 12.98, 15.1, 16.5, 17.36, 17.86, 18.15)

    parameters <- list(ring_centers = ring_centers,
                       patch_radii = patch_radii,
                       pix_per_patch = pix_per_patch,
                       g_cell_patch = g_cell_patch,
                       n_patches = n_patches)
  } else if(version == "expanded_fovea") {
    ring_centers <- round(c(
      0, 0.459800349073796, 0.973111993685347, 1.56256206738979, 2.32554893517623, 3.38989375592665, 4.90757077825768, 7.04333171846555, 9.97431500944187
    ), 3)
    patch_radii <- c(0.211956913605676, 0.247843435468120, 0.265468209143432, 0.323981864561015, 0.439005003225417, 0.625339817525009, 0.892337204806020, 1.24342373540184, 1.68755955557449)
    pix_per_patch <- c(2032, 2779, 3188, 4748, 8719, 17691, 36022, 69944, 128834)
    g_cell_patch <- c(1810, 2369, 2070, 1817, 1687, 1665, 1706, 1762, 1801)
    n_patches <- c(1, 6.85159149, 10.7275778, 12.73282105, 13.99479252, 15.06258584, 16.13548668, 17.24893549, 18.40780099)

    parameters <- list(ring_centers = ring_centers,
                       patch_radii = patch_radii,
                       pix_per_patch = pix_per_patch,
                       g_cell_patch = g_cell_patch,
                       n_patches = n_patches)

  } else {
    warning("no matching selection made")
  }

}
