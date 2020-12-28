#' Create the parameters used to define the efficiency maps.
#'
#' @return
#' @export
#'
#' @examples
create_linear_map_parameters <- function() {
  params <- expand.grid(
    sp_const = 0,
    #efficiency = seq(.793, .802, length.out = 4),
    #efficiency = c(.793, .796, .8, .802, .820, 1),
    #efficiency = c(.775, .78, .785, .79,.795, .8),
    efficiency = c(.78),
    contrast = .175,
    prior.scale = 1,
    scale.dist = c(1),
    n_rings = c(9),
    prior_type  = c("uniform", "polar"),
    map_prior = c("uniform", "polar")) %>%
    arrange(n_rings, efficiency)

  params <- merge(params, data.frame(subject = c("anqi", "rcw")))

  params[params$subject == "rcw", "params"] <- "[.92, .28, 1]"
  params[params$subject == "anqi", "params"] <- "[.87, .25, 1]"

  #params <- params %>% filter((prior_type == "uniform" & map_prior == "uniform"))
  params <- params %>%
    filter(!(prior_type == "uniform" & map_prior == "polar"))
  params <- params %>%
    filter(!(prior_type == "polar" & map_prior == "uniform"))

  return(params)
}
