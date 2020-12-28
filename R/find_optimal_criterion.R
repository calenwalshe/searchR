#' Find optimal criterion for a the max rule searcher
#'
#' @param model_search
#'
#' @return
#' @export
#'
#' @examples
find_optimal_criterion <- function(model_search) {
    # add spacing at target location
    params <- searchR::create_retinal_constants()
    spacing   <- searchR::watson_spacing(params$ring_centers, 0, binoc = 1, sp_scal = 0.943, f0 = 0)
    patch_rad <- .5 + params$patch_radii # taken from the table of values computed in the notes.

    f.custom_threshold <- purrr::partial(custom_add_threshold, spacing = spacing, patch_rad = patch_rad)
    search_with_spacing <- model_search %>% mutate(spacing = purrr::pmap(., function(stimX, stimY, tPresent, radius, ...) {
      cent.x <- floor(1200 * radius/8)
      cent.y <- floor(1200 * radius/8)
      spacing = ifelse(tPresent == 1, searchR::watson_spacing((stimX-cent.x)/120, (stimY-cent.y)/120, binoc = 1, sp_scal = 0.943, f0 = 0), -1)
      })) %>%
      tidyr::unnest(spacing)

    f.obj <- function(x) {
      model_import <- searchR::import_model(search_with_spacing, criterion = x)

      tmp_accuracy <- model_import %>%
        #dplyr::mutate(radius = 8) %>%
        f.custom_threshold(.)

      tmp_accuracy$correct <- ifelse(tmp_accuracy$tPresent == "absent" & tmp_accuracy$response == "absent", 1, 0)
      tmp_accuracy$correct <- with(tmp_accuracy, ifelse(tPresent == "present" & response == "present" & (sqrt((clickX - stimX)^2 + (clickY - stimY)^2) < tmp_accuracy$threshold), 1, tmp_accuracy$correct))
      return(1 - mean(tmp_accuracy$correct))
    }

    optim.val <- DEoptim::DEoptim(f.obj,
                                  lower = -10, upper = 10,
                                  DEoptim::DEoptim.control(trace = 50, reltol = 1e-14, steptol = 25, itermax = 3000))
    crit.val <- optim.val

    return(crit.val)
}

#' Determine the threshold cutoff to use for determining false hits.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
custom_add_threshold <- function(x, spacing, patch_rad) {
  library(dplyr)

  x$threshold <- Hmisc::approxExtrap(spacing, patch_rad, x$spacing)$y

  x[x$tPresent == "absent", "threshold"] <- NA

  return(x)
}
