#' Full summary
#'
#' @param response_df
#' @param parameter_label
#'
#' @return
#' @export
#'
#' @examples
plot_full_summary <- function(response_df, parameter_label = "Intercept") {
  figure.colours <-  ggthemes::ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]$`Classic 10`

  response_df <- ungroup(response_df)
  longer_df <- tidyr::pivot_longer(response_df,
                            cols = c("human", "model"),
                            names_to = "subject_type",
                            values_to = "summarized_response") %>%
    ungroup() %>%
    dplyr::select(sample_type, sp_const, map_prior, subject, subject_type, efficiency, efficiency_rank, radius, summarized_response, experiment) %>%
    group_by(subject_type) %>%
    mutate(efficiency = ifelse(subject_type == "human", 1, efficiency),
           efficiency_rank = ifelse(subject_type == "human", 1, efficiency_rank),
           sp_const = ifelse(subject_type == "human", 1, sp_const)) %>%
    unique()


  longer_df_expand <- longer_df %>%
    dplyr::select(-radius) %>%
    unnest(summarized_response)

  longer_df_expand$colour <- round(longer_df_expand$sp_const, 3)
  longer_df_expand$colour <- ifelse(longer_df_expand$subject_type == "human", -1, longer_df_expand$colour)
  colour_vals <- viridisLite::cividis(length(unique(longer_df_expand$colour)), end = .85)
  names(colour_vals) <- sort(unique(longer_df_expand$colour))
  colour_vals[names(colour_vals) == "-1"] <- "#ff0000"
  longer_df_expand$shape <- longer_df_expand$subject

  longer_df_expand$linetype <- round(longer_df_expand$sp_const,3)
  longer_df_expand$linetype <- ifelse(longer_df_expand$subject_type == "human", -1, longer_df_expand$linetype)

  longer_df_expand$subject_type <-
    paste0(longer_df_expand$subject_type, "_",
           round(longer_df_expand$efficiency), "_", round(longer_df_expand$sp_const,3))

  longer_df_expand <- ungroup(longer_df_expand)
  try({

  ggplot2::theme_set(theme_bw(base_size = 15))
  update_geom_defaults("point", list(size = .5))
  fig.hitmiss <- ggplot2::ggplot(longer_df_expand %>%
                          filter(type %in% c("hit", "miss", "fh")) %>%
                          group_by(dist.group, efficiency, subject, sample_type, subject_type) %>%
                          mutate(prop = prop / sum(prop)) %>%
                          filter(type %in% c("hit", "miss", "fh")) %>%
                          mutate(type = ifelse(type == "hit", "Hit", type)) %>%
                          mutate(type = ifelse(type == "miss", "Miss", type)) %>%
                          mutate(type = ifelse(type == "fh", "False Hit", type)) %>%
                          group_by(dist.group, type, efficiency, subject, sample_type, colour, sp_const, linetype, subject_type) %>%
                          summarize(prop = sum(prop), .groups = "keep"),
                        aes(x = (dist.group),
                            y = prop,
                            colour = factor(colour))) +
                            #colour = factor(subject_type, levels = c("human", "model", "ideal"), labels = c("Human", "Model", "Ideal")))) +
    geom_point() +
    geom_line() +
    facet_grid(rows = vars(subject = factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")), sample_type = factor(sample_type, levels = c("uniform", "polar"), labels = c("Uniform Sampling", "Polar Sampling"))),
               cols = vars(type = factor(type, levels = c("Hit", "Miss", "False Hit"), labels = c("Hit", "Miss", "False Hit")))) +
    theme(aspect.ratio = 1) +
    coord_cartesian(ylim = c(0, 1), xlim = c(0, 8)) +
    scale_y_continuous(breaks = c(0, .5, 1)) +
    scale_colour_manual(values = colour_vals) +
    xlab("Target Distance") +
    ylab("Proportion (Within Bins)") +
    ggtitle("Hits and Misses") +
    guides(colour = guide_legend(title = parameter_label))
})
  # False hits and false alarms by click
  try({
    fig.fh.click <- ggplot(longer_df_expand %>%
                                  filter(type %in% c("fh", "fa")) %>%
                                  group_by(subject_type, sp_const, subject, sample_type, type, colour, linetype, dist.group.click) %>%
                                  summarise(prop = sum(prop), .groups = "keep") %>%
                                  ungroup() %>%
                                  mutate(type = ifelse(type == "fh", "False Hit", type)) %>%
                                  mutate(type = ifelse(type == "fa", "False Alarm", type)),
                           aes(x = (dist.group.click),
                               y = prop,
                               colour = factor(colour),
                               )) +
    geom_point() +
    geom_line() +
    theme(aspect.ratio = 1) +
    coord_cartesian(ylim = c(0, .04), xlim = c(0, 8)) +
    scale_y_continuous(breaks = c(0, .02, .04)) +
    scale_colour_manual(values = colour_vals) +
    facet_grid(rows = vars(sample_type = factor(sample_type, levels = c("uniform", "polar"),
                                                labels = c("Uniform Sampling", "Polar Sampling"))),
               cols = vars(factor(type, levels = c("False Hit", "False Alarm")),
                           factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")))) +
    labs(colour = "Parameter") +
    xlab("Click Location") +
    ylab("Proportion (All Trials)") +
    ggtitle("False Hits") +
    guides(colour = FALSE, linetype = FALSE)

  })

  # Bar Graph Responses
  try({fig.cr <- ggplot(longer_df_expand %>%
                          mutate(type = ifelse(type == "cr", "Correct Rejection", type),
                                 type = ifelse(type == "hit", "Hit", type),
                                 type = ifelse(type == "fh", "False Hit", type),
                                 type = ifelse(type == "fa", "False Alarm", type),
                                 type = ifelse(type == "miss", "Miss", type)) %>%
                          group_by(subject_type, subject, sample_type, colour, sp_const, linetype, type) %>%
                          summarize(prop = sum(prop), .groups = "keep"),
                        aes(x = factor(type, levels = c("Hit", "Miss", "False Hit", "False Alarm", "Correct Rejection"),
                                       labels = c("Hit", "Miss", "False\nHit", "False\nAlarm", "Correct\nRejection")),
                            y = prop,
                            fill = factor(colour),
                            colour = factor(colour))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    #theme(aspect.ratio = 1) +
    coord_cartesian(ylim = c(0, .5)) +
    facet_grid(rows = vars(sample_type = factor(sample_type, levels = c("uniform", "polar"),
                                                labels = c("Uniform Sampling", "Polar Sampling"))), cols = vars(factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")))) +
    scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5)) +
    scale_colour_manual(values = colour_vals) +
    scale_fill_manual(values = colour_vals) +
    labs(colour = "Observer") +
    guides(fill = guide_legend(title = parameter_label), colour = FALSE) +
    ylab("Proportion") +
    xlab('') +
    ggtitle("Task Performance")
  })

  # Bar Graph Responses
  try({fig.pc <- ggplot(longer_df_expand %>%
                          filter(type %in% c("hit", "cr")) %>%
                          mutate(type = ifelse(type == "cr", "Correct Rejection", type),
                                 type = ifelse(type == "hit", "Hit", type)) %>%
                          group_by(subject_type, subject, colour, linetype, sample_type) %>%
                          summarize(prop = sum(prop), .groups = "keep"),
                        aes(x = factor(colour),
                            y = prop,
                            fill = factor(linetype),
                            colour = factor(colour))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    #theme(aspect.ratio = 1) +
    scale_fill_manual(values = colour_vals) +
    scale_colour_manual(values = colour_vals) +
    coord_cartesian(ylim = c(.5, 1)) +
    facet_grid(rows = vars(sample_type = factor(sample_type, levels = c("uniform", "polar"),
                                                labels = c("Uniform Sampling", "Polar Sampling"))), cols = vars(subject = factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")))) +
    scale_y_continuous(breaks = c(.5, .6, .7, .8, .9, 1)) +
    guides(fill = FALSE, colour = FALSE) +
    ylab("Percent Correct") +
    ylab("Proportion") +
    xlab("Parameter Value") +
    ggtitle("Task Performance")
  })

  # gain_maps
  try({
    gain_map <- response_df %>%
      dplyr::select(sp_const, efficiency, map_prior, subject, scale.dist, optimal_criterion, data) %>% unnest(data) %>%
      mutate(scaled_dprime = y.scale * y.dprime)

    gain_map$colour <- as.character(round(gain_map$sp_const, 3))

    colour_vals_gain <- colour_vals[names(colour_vals) != "human"]

    #gain_map$colour <- ifelse(gain_map$subject_type == "human", "human", gain_map$colour)

            # mean_pres = apply(replicate(1000, {((rnorm(1)) + y.dprime*y.scale)  * y.dprime*y.scale - 1/2 * (y.scale * y.dprime)^2 + -14.8786}), 1, mean),
    fig.gain <- ggplot(gain_map, aes(x = c_deg, y = y.scale,
                                     colour = factor(colour))) +
    #geom_point() +
    geom_line() +
    theme(aspect.ratio = 1) +
    coord_cartesian(ylim = c(0, 1)) +
    facet_grid(rows = vars(sample_type = factor(sample_type, levels = c("uniform", "polar"),
                                                labels = c("Uniform Sampling", "Polar Sampling"))), cols = vars(subject = factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")))) +
    scale_y_continuous(breaks = c(0, .25, .5, .75, 1)) +
    scale_colour_manual(values = colour_vals_gain) +
    ylab("Gain") +
    guides(colour = FALSE) +
    xlab("Degrees") +
    ggtitle("Gain Map")

    fig.dprime <- ggplot(gain_map, aes(x = c_deg, y = scaled_dprime,
                                     colour = factor(colour))) +
      #geom_point() +
      geom_line() +
      geom_line(aes(x = c_deg, y = y.dprime), colour = "black") +
      theme(aspect.ratio = 1) +
      coord_cartesian(ylim = c(0, 7.5)) +
      facet_grid(rows = vars(sample_type = factor(sample_type, levels = c("uniform", "polar"),
                                                  labels = c("Uniform Sampling", "Polar Sampling"))), cols = vars(subject = factor(subject, levels = c("rcw", "anqi"), labels = c("rcw", "az")))) +
      scale_y_continuous(breaks = c(0, 2, 4, 6)) +
      scale_colour_manual(values = colour_vals_gain) +
      ylab("Scaled d'") +
      guides(colour = FALSE) +
      xlab("Degrees") +
      ggtitle("Scaled d' map")

    #fig.gain <- cowplot::plot_grid(fig.gain, fig.dprime, nrow = 2)
  })

  fig.out <- list(fig.hitmiss,  fig.gain, fig.pc, fig.fh.click, fig.dprime, fig.cr)

  return(fig.out)
}
