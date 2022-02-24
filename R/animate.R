#' Create animated gif of error bars
#'
#' @param mc An mc object from the mcdata package.
#' @param demographic demographic to plot

animate_error_bar <- function(mc_filtered, demographic = FALSE) {
  mc_filtered <- mc_filtered[mc_filtered[["age"]] <= 90, ]
  for (month in as.character(seq(lubridate::ymd("2013-02-01"),
                                 lubridate::floor_date(Sys.Date(), "month"),
                                 by = "week"))) {
    print(month)
    mc <- mc_filtered[mc_filtered[["created_at"]] <= (lubridate::ymd(month)), ]
    n <- nrow(mc)
    if (demographic == FALSE) {
      temp <- mc %>%
        dplyr::filter(., !is.na(age)) %>%
        dplyr::group_by(age) %>%
        dplyr::summarise(mean = mean(totalcorrect),
                  sd = sd(totalcorrect),
                  n = dplyr::n()) %>%
        dplyr::mutate(se = sd / sqrt(n),
               lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
               upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
               group = 1,
               month = as.Date(month))
      temp[["total_n"]] <-  n
    } else {
      for (option in unique(mc[[demographic]])) {
        if (!is.na(option) && option != "unknown") {
          temp <- mc[mc[[demographic]] == option, ] %>%
            dplyr::filter(., !is.na(age)) %>%
            dplyr::group_by(age) %>%
            dplyr::summarise(mean = mean(totalcorrect),
                      sd = sd(totalcorrect),
                      n = dplyr::n()) %>%
            dplyr::mutate(se = sd / sqrt(n),
                   lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
                   upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se,
                   group = option,
                   month = as.Date(month))
          temp[["total_n"]] <-  n
          ifelse(exists("mc_full_line"),
                 mc_full_line <- rbind(mc_full_line, temp),
                 mc_full_line <- temp)
        }
      }

    }
    ifelse(exists("mc_full_line"), mc_full_line <- rbind(mc_full_line, temp),
           mc_full_line <- temp)
  }

  get_n <- function(date){
    n <- mc_full_line %>%
      dplyr::filter(month == date) %>%
      tidyr::drop_na(total_n)
    format(n[["total_n"]][1], big.mark = ",")
  }
  pd <- ggplot2::position_dodge(0.1)
  mc_full_line[["group"]] <- as.factor(mc_full_line[["group"]])
  p <- ggplot2::ggplot(mc_full_line,
                      ggplot2::aes(x = age, y = mean, colour = group, group = group)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_ci, ymax = upper_ci), width = 0.1, position=pd) +
    ggplot2::geom_line(position=pd) +
    ggplot2::geom_point(position=pd) +
    my_theme() +
    ggplot2::labs(x = "Age", y = "Score") +
    gganimate::transition_time(month) +
    ggplot2::labs(title = "Week: {frame_time}",
    subtitle = "n: {get_n(frame_time)}") +
    ggplot2::scale_colour_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0,36)) +
    ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5))
  name <- paste0(demographic, "_error_bar_animation.png")
  animation <- gganimate::animate(p, nframes = length(as.character(seq(lubridate::ymd("2013-02-01"), lubridate::floor_date(Sys.Date(), "month"), by = "week"))))
  gganimate::anim_save(paste0(demographic, "_error_bar_weekly.gif"), animation)
}
