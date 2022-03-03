#' Generate a demographic barplot from an mc object
#'
#' @param mc An mc object from the mcdata package.
#' @param demographic A string of the demographic you wish to plot.
#' @export

plot_demographic_barplot <- function(mc, demographic,
                                    title = "", subset = FALSE) {
  mc <- mc[mc[["age"]] <= 90, ]
  df <- as.data.frame(table(mc[c("age", demographic)]))
  df[["age"]] <- as.numeric(as.character(df[["age"]]))
  if ("TRUE" %in% unique(mc[[demographic]])) {
    totals <- df %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(total = sum(Freq))
    df <- merge(df, totals)
    df[["percentages"]] <- df[["Freq"]] / df[["total"]]
    df <- df[df[[demographic]] == TRUE, ]
    p <- ggplot2::ggplot(df,
                         ggplot2::aes_string(x = "age", y = "percentages"))
  } else{
    totals <- df %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(total = sum(Freq))
    df <- merge(df, totals)

    if (subset != FALSE) {
      df <- df[df[[demographic]] %in% subset, ]
    }
    df[["percentages"]] <- df[["Freq"]] / df[["total"]]
    df[[demographic]] <- as.character(df[[demographic]])
    df[[demographic]] <- stringr::str_wrap(df[[demographic]], 10)
    p <- ggplot2::ggplot(df,
                         ggplot2::aes_string(x = "age",
                                        y = "percentages", fill = demographic))
  }
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    my_theme() +
    ggplot2::labs(x = "Age", y = "Percentage of Participants", title = title) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                       breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5)) +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())

  name <- paste0(demographic, "_barplot.png")
  ggplot2::ggsave(name, p, "png")
  return(p)
}

#' Contactable Lineplot
#'
#' Generate a line plot of the percentage of contactable participants from ages 18-90, optionally by a specified demographic
#'
#' @param mc An mc object from the mcdata package.
#' @param demographic (optional) Default = FALSE. A string of the demographic you wish to plot.
#' @export
plot_percent_contactable <- function(mc, demographic = FALSE){
  mc <- mc[mc[["age"]] <= 90, ]
  if (demographic == FALSE){
    df <- as.data.frame(table(mc[c("age", "contactable")]))
    totals <- df %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(total = sum(Freq))
  } else {
    df <- as.data.frame(table(mc[c("age", "contactable", demographic)]))
    df[["demographic"]] <- df[[demographic]]
    totals <- df %>%
      dplyr::group_by(age, demographic) %>%
      dplyr::summarise(total = sum(Freq))
    df[[demographic]] <- as.character(df[[demographic]])
    df[[demographic]] <- stringr::str_wrap(df[[demographic]], 10)
  }
  df[["age"]] <- as.numeric(as.character(df[["age"]]))
  df <- merge(df, totals)
  df[["percentages"]] <- df[["Freq"]] / df[["total"]]
  df <- df[df$contactable == TRUE,]
  p <- ggplot2::ggplot(df,
                       ggplot2::aes_string(x = "age", y = "percentages",
                                           color = ifelse(demographic == FALSE, "contactable", demographic)))

  p <- p +
    ggplot2::geom_line() +
    my_theme() +
    ggplot2::labs(x = "Age", y = "Percentage of Participants") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5)) +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  p
}
