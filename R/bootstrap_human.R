#' Bootstrap human responses
#'
#' @param human_search
#'
#' @return
#' @export
#'
#' @examples
bootstrap_search <- function(human_search, nsamples = 10) {
  human_search <- humansearchdata::generic_import() %>% filter(subject == "rcw", experiment == "uniform")

  resampled_results <- parallel::mclapply(1:nsamples, function(x) {
    human_search %>%
      group_by(subject, experiment, tPresent) %>%
      sample_frac(size = 1, replace = TRUE) %>%
      humansearchdata::add_accuracy(.) %>%
      humansearchdata::add_threshold(.) %>%
      searchR::summary_search(.) %>%
      mutate(iter_resamp = x)
    }, mc.cores = 12)

  resampled_results.1 <- do.call(rbind, resampled_results)

  sd_prop_hits <- resampled_results.1 %>% group_by(dist.group, dist.group.click, type, iter_resamp) %>%
    filter(type %in% c("hit", "miss", "fh")) %>%
    group_by(dist.group, type, iter_resamp) %>%
    summarize(prop = sum(prop)) %>%
    group_by(dist.group, iter_resamp) %>%
    mutate(prop = prop / sum(prop)) %>%
    group_by(dist.group, type) %>%
    summarize(sd_prop = sd(prop))

  sd_prop_clicks <- resampled_results.1 %>%
    filter(type %in% c("fh", "fa")) %>%
    group_by(type, dist.group.click, iter_resamp) %>%
    summarise(prop = sum(prop), .groups = "keep") %>%
    group_by(dist.group.click, type) %>%
    summarize(sd_prop = sd(prop))

  sd_prop_pc <- resampled_results.1 %>% group_by(dist.group, dist.group.click, type, iter_resamp) %>%
    filter(type %in% c("cr", "hit")) %>%
    group_by(iter_resamp) %>%
    summarize(prop = sum(prop)) %>%
    ungroup() %>%
    summarize(sd_prop = sd(prop))

  list(sd_hits = sd_prop_hits, sd_clicks = sd_prop_clicks, sd_pc = sd_prop_pc)
}
