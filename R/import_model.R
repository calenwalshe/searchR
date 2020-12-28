#' import_model
#'
#' @param data
#' @param criterion
#'
#' @return
#' @export
#'
#' @examples
import_model <- function(data, criterion = 0) {

  rename  <- dplyr::rename

  bgScale <- 8 / data$radius[1] # df must contain a radius value.

  model.results <- data
  ppd     <- 60
  cent.x <- floor(1200 / bgScale) # DANGER, this must be the value of the matrix size used to do the search. Current it is 2400 x 2400. Run the code and check if need be.
  cent.y <- floor(1200 / bgScale)

  model.results <- model.results %>% mutate(response = ifelse(slpmx > criterion, 1, 0),
                                            clickX       = ifelse(response == 1, (xmax - cent.x) / 120, NA),
                                            clickY       = ifelse(response == 1, (ymax - cent.y) / 120, NA),
                                            stimX       = ifelse(tPresent == 1, (stimX - cent.x) / 120, NA),
                                            stimY       = ifelse(tPresent == 1, (stimY - cent.y) / 120, NA),
                                            tPresent = ifelse(tPresent == 1, "present", "absent"),
                                            subject = "model",
                                            trial = seq_along(trial)
  ) %>% as_tibble()

  model.results.1          <- model.results
  model.results.1$response <- factor(ifelse(model.results.1$response == 1, "present", "absent"))

  model.results.2 <- left_join(model.results.1, model.results %>%
                                 group_by(trial) %>%
                                 nest(),
                               by = c("trial"))

  model.results.2 <- model.results.2 %>%
    rename(data_raw = data)

  model.results.2$criterion <- criterion

  return(model.results.2)
}
