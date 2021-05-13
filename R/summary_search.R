#' Formats the search data into a marginaled response table. Summary search statistics.
#'
#' @param imported_search
#'
#' @return
#' @export
#'
#' @examples
summary_search <- function(imported_search) {
  library(dplyr)

  imported_search_1 <- imported_search %>%
    tidyr::gather(type, response, c("hit", "miss", "fa", "fh", "cr")) %>%
    filter(response == 1) %>%
    dplyr::select(-response)

  imported_search_2 <- imported_search_1 %>%
    mutate(dist.cent = ifelse(dist.cent >= max(radius), max(radius), dist.cent),
           dist.cent = ifelse(type == "fa", NA, dist.cent),
           type      = ifelse(type == "fh", "fh", type),
           click.dist.cent = ifelse(click.dist.cent >= max(radius), max(radius), click.dist.cent),
           click.dist.cent = ifelse(type == "hit", -1, click.dist.cent)) %>%
    ungroup()

  imported_search_3 <- imported_search_2 %>%
    mutate(max.radius = max(radius),
           dist.group = cut(dist.cent, include.lowest = TRUE,
                            breaks = seq(0, max.radius[1], length.out = 6),
                            labels = seq(0, max.radius[1], length.out = 5)),
           dist.group.click = cut(click.dist.cent, include.lowest = TRUE,
                                  breaks = seq(0, max.radius[1], length.out = 6),
                                  labels = seq(0, max.radius[1], length.out = 5)),
           dist.group = ifelse(is.na(dist.group), -1, dist.group),
           dist.group.click = ifelse(is.na(dist.group.click), -1, dist.group.click)) %>%
    group_by(radius, dist.group, dist.group.click, type, .drop = FALSE) %>%
    dplyr::summarize(count = n()) %>%
    ungroup()

  imported_search_4 <- imported_search_3 %>%
    mutate(count = replace_na(count, 0),
           prop = count / sum(count))


  imported_search_5 <- imported_search_4 %>%
    mutate(dist.group = ifelse(dist.group > 0, - 1/2 * radius/5 + dist.group * radius/5, dist.group)) %>%
    mutate(dist.group.click = ifelse(dist.group.click > 0,
                                     -1/2 * radius/5 + dist.group.click * radius/5,
                                     dist.group.click))

  imported_search_6 <- fill_missing_search(imported_search_5)

  imported_search_6$dist.group.click <- ifelse(imported_search_6$type == "hit", imported_search_6$dist.group, imported_search_6$dist.group.click)

return(imported_search_6)
}

#' Title
#'
#' @param tbl
#'
#' @return
#'
#' @examples
fill_missing_search <- function(tbl) {

  # combine all levels
  radius <- tbl$radius[1]

  dist.group.vals <- - 1/2 * radius/5 + 1:5 * radius/5
  dist.group.click.vals <- - 1/2 * radius/5 + 1:5 * radius/5

  hit.levels  <- data.frame(type = "hit", dist.group.click = -1, dist.group = dist.group.vals)
  miss.levels <- expand.grid(type = "miss", dist.group.click = -1, dist.group = dist.group.vals)
  fh.levels  <- expand.grid(type = "fh", dist.group.click = dist.group.click.vals, dist.group = dist.group.vals)
  cr.levels <- data.frame(type = "cr", dist.group.click = -1, dist.group = -1)
  fa.levels  <- expand.grid(type = "fa", dist.group.click = dist.group.click.vals, dist.group = -1)

  all.levels <- bind_rows(hit.levels, miss.levels, fh.levels, fa.levels, cr.levels)
  all.levels$radius <- radius
  n_trials <- sum(tbl$count)

  out.dat <- full_join(tbl, all.levels, by = c("radius", "type", "dist.group.click", "dist.group"))
  out.dat$prop <- ifelse(is.na(out.dat$prop), 0, out.dat$prop)

  return(out.dat)
}


