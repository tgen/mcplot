#' Plot Age Barplot
#'
#' @param mc An mc object from the mcdata package.
#' @param demographic A string of the demographic you wish to plot.
#' @export

plot_age_barplot <- function(mc,
                             title = "",
                             percentage = TRUE,
                             age_decade = FALSE,
                             last_month = FALSE,
                             path = FALSE,
                             name = "") {
  mc <- mc[mc[["age"]] <= 90, ]
  mc <- mc[mc[["age"]] >= 18, ]
  if (last_month == TRUE){
    mc <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]
    mc <- mc[as.Date(mc[["created_at"]]) > lubridate::floor_date(Sys.Date(), "month") - months(1), ]
  }
  if (age_decade == FALSE) {
    mc[["age"]] <- as.numeric(as.character(mc[["age"]]))
    df <- as.data.frame(table(mc[c("age")]))
  } else {
    df <- as.data.frame(table(mc[c("age_decade")]))
    df[["age"]] <- df[["age_decade"]]
  }
  df[["totals"]] <- sum(df[["Freq"]])
  df[["percentages"]] <- df[["Freq"]]/df[["totals"]]
  df[["age"]] <- df[["Var1"]]
  if (age_decade == FALSE){
    df[["age"]] <- as.numeric(as.character(df[["age"]]))
  }
  if (percentage == TRUE) {
    p <- ggplot2::ggplot(df,
                          ggplot2::aes_string(x = "age", y = "percentages"))
   } else {
    p <- ggplot2::ggplot(df,
                          ggplot2::aes_string(x = "age", y = "Freq"))
   }
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    my_theme() +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  if (percentage == TRUE){
    p <- p +
      ggplot2::labs(y = "Percentage of Participants", title = title) +
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = scales::pretty_breaks(n = 10))
  } else {
    p <- p +
      ggplot2::labs(y = "Number of Participants", title = title)
  }

  if (age_decade == FALSE){
    p <- p +
      ggplot2::labs(x = "Age") +
      ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5))

  } else {
    p <- p +
      ggplot2::labs(x = "Age Decade")
  }

  if (percentage == TRUE){
    name <- paste0(name,"age_percentage")
  } else {
    name <- paste0(name,"age_N")
  }

  if (last_month == TRUE){
    name <- paste0(name, "_last_month")
  }

  name <- paste0(name, "_barplot.png")
  if (path != FALSE){
    name <- paste0(path, "/", name)
  }
  ggplot2::ggsave(name, p, "png")
  return(p)
}

#' Generate a demographic barplot from an mc object
#'
#' @param mc An mc object from the mcdata package.
#' @param demographic A string of the demographic you wish to plot.
#' @export

plot_demographic_barplot <- function(mc, demographic,
                                    title = "", subset = FALSE,
                                    percentage = TRUE,
                                    age_decade = FALSE,
                                    path = "./",
                                    name = "",
                                    upload = FALSE) {
  mc <- mc[mc[["age"]] <= 90, ]
  mc <- mc[mc[["age"]] >= 18, ]
  if (age_decade == FALSE) {
    mc[["age"]] <- as.numeric(as.character(mc[["age"]]))
    df <- as.data.frame(table(mc[c("age", demographic)]))
    df[["age"]] <- as.numeric(as.character(df[["age"]]))
  } else {
    df <- as.data.frame(table(mc[c("age_decade", demographic)]))
    df[["age"]] <- df[["age_decade"]]
  }

  if ("TRUE" %in% unique(mc[[demographic]])) {
    totals <- df %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(total = sum(Freq))
    df <- merge(df, totals)
    df[["percentages"]] <- df[["Freq"]] / df[["total"]]
    df <- df[df[[demographic]] == TRUE, ]
    if (percentage == TRUE) {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_string(x = "age", y = "percentages"))
    } else {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_string(x = "age", y = "Freq"))
    }

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
    if (percentage == TRUE) {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_string(x = "age", y = "percentages",
                                               fill = demographic))
    } else {
      p <- ggplot2::ggplot(df,
                           ggplot2::aes_string(x = "age", y = "Freq",
                                               fill = demographic))
    }
  }
  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    my_theme() +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  if (percentage == TRUE){
    p <- p +
      ggplot2::labs(y = "Percentage of Participants", title = title) +
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = scales::pretty_breaks(n = 10))
  } else {
    p <- p +
      ggplot2::labs(y = "Number of Participants", title = title)
  }

  if (age_decade == FALSE){
    p <- p +
        ggplot2::labs(x = "Age") +
        ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5))

  } else {
    p <- p +
      ggplot2::labs(x = "Age Decade")
  }


  if (subset == FALSE){
    name <- paste0(name, demographic)
  } else {
    name <- paste0(name, demographic, subset)
  }

  if (percentage == TRUE){
    name <- paste0(name, "_percentage")
  } else {
    name <- paste0(name, "_N")
  }

  name <- paste0(path,name, "_barplot.png")
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
plot_percent_contactable <- function(mc, demographic = FALSE,
                                     demographic_subset = FALSE){
  mc <- mc[mc[["age"]] <= 90, ]
  if (demographic == FALSE){
    df <- as.data.frame(table(mc[c("age", "contactable")]))
    totals <- df %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(total = sum(Freq))
  } else {
    if (demographic_subset != FALSE){
      mc <- mc[mc[[demographic]] %in% demographic_subset, ]
      mc[[demographic]] <- as.factor(as.character(mc[[demographic]]))
    }
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
    ggplot2::labs(x = "Age", y = "Percentage of Participants") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5)) +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  ifelse(demographic == FALSE,
         p <- p + mcdata::mc_theme(),
         p <- p + my_theme("top_left"))
  p
}
