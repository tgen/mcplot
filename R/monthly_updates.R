#' Participation N Line
#'
#' @param mc mcdata object
#' @export
plot_participation_line <- function(mc, monthly = TRUE, path = FALSE) {
  mc <- dplyr::distinct(mc, user_id, .keep_all = TRUE)
  baseline_count <- sum(as.Date(mc[["created_at"]]) < as.Date("2021-10-01"),
                        na.rm = TRUE)
  mc[["month"]] <- lubridate::floor_date(mc[["created_at"]], unit = "month")
  mc <- mc[as.Date(mc[["created_at"]]) >= as.Date("2021-10-01"), ]
  mc <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(),
                                                               "month"), ]

  mc_counts <- as.data.frame(table(mc[["month"]]))

  #Get mc totals for each month in barplot
  mc_counts[["total"]] <- format(baseline_count + cumsum(mc_counts[["Freq"]]),
                                 big.mark = ",")
  mc_counts[["Var1"]] <- as.Date(mc_counts[["Var1"]])

  if (monthly == TRUE){
    p <- ggplot2::ggplot(data = mc_counts,
                         ggplot2::aes(x = Var1, y = Freq,
                                      group = 1, label = total)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 5500, 500),
                                  limits = c(0, 5500)) +
      ggplot2::labs(x = "Month",
                    y = "Number of Participants Recruited (Monthly)")
  } else {
      p <- ggplot2::ggplot(data = mc_counts,
                           ggplot2::aes(x = Var1, y = total,
                                        group = 1, label = total)) +
        ggplot2::labs(x = "Month",
                      y = "Total Number of Participants")

  }


  p <- p +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_text(hjust = 0.5, vjust = -1) +
    ggplot2::theme(text = ggplot2::element_text(size = 15)) +
    modified_theme() +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
  if (path != FALSE){
    save_path <- paste0(path, "/", Sys.Date(), "_monthly_participation_lineplot.png")
  } else {
    save_path <- paste0(Sys.Date(), "_monthly_participation_lineplot.png")
  }
  ggplot2::ggsave(save_path,
                  p, width = 8, height = 5)
  p
}

#' Participation N Barplot
#'
#' @param mc mcdata object
#' @export
plot_participation_barplot <- function(mc, path = FALSE) {
  mc <- dplyr::distinct(mc, user_id, .keep_all = TRUE)
  baseline_count <- sum(as.Date(mc[["created_at"]]) < as.Date("2021-10-01"),
                        na.rm = TRUE)
  mc[["month"]] <- lubridate::floor_date(mc[["created_at"]], unit = "month")
  mc <- mc[as.Date(mc[["created_at"]]) >= as.Date("2021-10-01"), ]
  mc <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(),
                                                               "month"), ]

  mc_counts <- as.data.frame(table(mc[["month"]]))

  #Get mc totals for each month in barplot
  mc_counts[["total"]] <- format(baseline_count + cumsum(mc_counts[["Freq"]]),
                                 big.mark = ",")
  mc_counts[["Var1"]] <- as.Date(mc_counts[["Var1"]])

  p <- ggplot2::ggplot(data = mc_counts,
                       ggplot2::aes(x = Var1, y = Freq,
                                    label = format(Freq, big.mark = ","))) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(hjust = 0.5, vjust = -1) +
    ggplot2::theme(text = ggplot2::element_text(size = 15)) +
    modified_theme() +
    ggplot2::labs(x = "Month",
                  y = "Number of Participants Recruited (Monthly)") +
    ggplot2::scale_y_continuous(breaks = seq(0, 5500, 500),
                                limits = c(0, 5500)) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")

  if (path != FALSE){
    save_path <- paste0(path, "/", Sys.Date(), "_monthly_participation_barplot.png")
  } else {
    save_path <- paste0(Sys.Date(), "_monthly_participation_barplot.png")
  }
  ggplot2::ggsave(save_path,
                  p, width = 8, height = 5)
  p
}


#' Recruitment Projection Line
#'
#' @param mc mcdata object
#' @param tart_date (optional) Default = "2020-01-01", Start date of projection
#' @param end_date (optional) Default = "2030-01-01", End date of projection
#' @export
plot_projected_participants <- function(mc, start_date = "2020-01-01",
                                        end_date = "2030-01-01",
                                        options = FALSE,
                                        months_to_average = 6,
                                        goal = FALSE) {
  mc <- dplyr::distinct(mc, user_id, .keep_all = TRUE)
  baseline_count <- sum(as.Date(mc[["created_at"]]) < as.Date(start_date),
                        na.rm = TRUE)

  mc[["month"]] <- lubridate::floor_date(mc[["created_at"]], unit = "month")
  mc <- mc[as.Date(mc[["created_at"]]) >= as.Date(start_date), ]
  mc <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(),
                                                              "month"), ]

  mc_counts <- as.data.frame(table(mc[["month"]]))
  six_mos_table <- mc_counts[(nrow(mc_counts) - months_to_average):nrow(mc_counts), ]
  average_rate <- mean(six_mos_table[["Freq"]])

  line_df <- data.frame(Month = as.character(mc_counts[["Var1"]]),
                            Total = cumsum(mc_counts[["Freq"]]),
                            Record = "Past Data", stringsAsFactors = FALSE)
  start_date <- as.Date(line_df[["Month"]][[length(line_df[["Month"]])]])

  time_range <- lubridate::interval(lubridate::ymd(start_date),
                          lubridate::ymd(end_date))
  time_range <- time_range %/% months(1)
  dates <- start_date + months(1:time_range)
  if (options == TRUE){
    for (x in c(1, 1.25, 1.5, 2)) {
      dates_values <- line_df[["Total"]][[length(line_df[["Total"]])]] +
        (x * average_rate * 1:time_range)
      df <- data.frame(Month = as.character(dates),
                       Total = dates_values,
                       Record = as.character(x), stringsAsFactors = FALSE)
      ifelse(exists("long_df"),
             long_df <- rbind(long_df, df),
             long_df <- df)
    }
  } else {
    dates_values <- line_df[["Total"]][[length(line_df[["Total"]])]] +
      (average_rate * 1:time_range)
    long_df <- data.frame(Month = as.character(dates),
                     Total = dates_values,
                     Record = "Projected", stringsAsFactors = FALSE)
  }

  rm(dates)
  rm(dates_values)
  final_line_df <- rbind(line_df, long_df)
  final_line_df[["Total"]] <- final_line_df[["Total"]] + baseline_count
  final_line_df[["Month"]] <- as.Date(final_line_df[["Month"]])

  p <- ggplot2::ggplot(data = final_line_df,
                       ggplot2::aes(x = Month,
                                    y = Total, group = Record,
                                    color = Record)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    my_theme("bottom_right") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0)) +
    ggplot2::scale_x_date(date_breaks = "1 year",
                          date_minor_breaks = "1 month",
                          date_labels = "%Y") +
    ggplot2::scale_y_continuous(label = scales::comma) +
    ggplot2::labs(x = "Year", y = "Total Participants")
  if (options == TRUE){
    p <- p +
      ggplot2::geom_vline(xintercept = as.Date("2021-10-01"),
                          color = "black",
                          linetype = "dashed")
  }

  ggplot2::ggsave(paste0(Sys.Date(),
                         "_monthly_overall_participation_projection.png"))
  p
}


#' Monthly demographic Table
#'
#' @param mc mcdata object
#' @param current_month (optional) Default = Current month. The later month used for comparison
#' @param last_month (optional) Default = Last month. The earlier month used for comparison
#' @importFrom lubridate %within%
#' @export
monthly_demographic_comparison <- function(mc,
                                           current_month = lubridate::floor_date(Sys.Date(), "month"),
                                           last_month = current_month - months(1),
                                           path = FALSE) {
  mc <- dplyr::distinct(mc, user_id, .keep_all = TRUE)
  month1 <- lubridate::floor_date(last_month, "month")
  month2 <- lubridate::floor_date(current_month, "month")

  mc_last_month <- mc[mc[["created_at"]] %within%
                      lubridate::interval(start = lubridate::ymd(month1),
                      lubridate::ymd(month1) + months(1)-lubridate::days(1)), ]


  mc_this_month<- mc[mc[["created_at"]] %within% lubridate::interval(start = lubridate::ymd(month2),
                                                                    lubridate::ymd(month2)+months(1)-lubridate::days(1)), ]

  demographics <- mcdata::report_demographics(mcs = list(mc_last_month, mc_this_month),
                                     labels = list(month1, month2))
  demographics[["Delta"]] <- round(as.numeric(gsub("%", "",demographics[[paste0(month2, "_prop")]]))-as.numeric(gsub("%", "",demographics[[paste0(month1, "_prop")]])),2)
  demographics[["Delta"]] <- paste0(as.character(demographics[["Delta"]]),"%")

  if (path != FALSE){
    name <- paste0(path, "/", month1, "_",month2,"_demographics.csv")
  } else {
    name <- paste0(month1, "_",month2,"_demographics.csv")
  }

  write.csv(demographics,name, row.names = FALSE)
}

#' NIH Plots
#'
#' @param prod TRUE or FALSE if being used in production environment
#' @param upload FALSE or GDrive ID for upload location
#' @export
nih_plots <- function(prod = FALSE, upload = FALSE) {

  convert_set_notation <- function(mc){
    mc$age_decade_reformatted <- NA
    mc <- mc %>%
      filter(!is.na(mc$age_decade))
    mc[mc[["age_decade"]] == "[18,20)",]$age_decade_reformatted <- "18-19"
    mc[mc[["age_decade"]] == "[20,30)",]$age_decade_reformatted <- "20-29"
    mc[mc[["age_decade"]] == "[30,40)",]$age_decade_reformatted <- "30-39"
    mc[mc[["age_decade"]] == "[40,50)",]$age_decade_reformatted <- "40-49"
    mc[mc[["age_decade"]] == "[50,60)",]$age_decade_reformatted <- "50-59"
    mc[mc[["age_decade"]] == "[60,70)",]$age_decade_reformatted <- "60-69"
    mc[mc[["age_decade"]] == "[70,80)",]$age_decade_reformatted <- "70-79"
    mc[mc[["age_decade"]] == "[80,90)",]$age_decade_reformatted <- "80-89"
    mc$age_decade <- mc$age_decade_reformatted
    mc
  }

  mc_tidy <- mcdata::mc_download(datatype = "tidy")
  mc_tidy <- convert_set_notation(mc_tidy)
  dir_name <- paste0("nih_plots")
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }

  mc_tidy <- dplyr::distinct(mc_tidy, user_id, .keep_all = TRUE)
  plot_participation_line(mc_tidy, monthly = FALSE, path = dir_name)
  plot_participation_barplot(mc_tidy, path = dir_name)
  plot_age_barplot(mc_tidy, age_decade = TRUE, path = dir_name)
  monthly_demographic_comparison(mc_tidy, path = dir_name)

  mc <- mc_tidy
  mc <- mc[mc[["age"]] <= 90, ]
  mc <- mc[mc[["age"]] >= 18, ]
  mc[["timeframe"]] <- "All Time"
  mc_tmp <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]
  mc_tmp <- mc_tmp[as.Date(mc_tmp[["created_at"]]) > lubridate::floor_date(Sys.Date(), "month") - months(1), ]
  mc_tmp[["timeframe"]] <- "Last Month"
  mc <- rbind(mc, mc_tmp)

  df <- as.data.frame(table(mc[c("age_decade", "timeframe")]))
  df[["age"]] <- df[["age_decade"]]

  df <- df %>%
    dplyr::group_by(timeframe) %>%
    dplyr::mutate(totals = sum(Freq))
  df[["percentages"]] <- df[["Freq"]]/df[["totals"]]

  p <- ggplot2::ggplot(df,
                       ggplot2::aes_string(x = "age", y = "percentages"))

  p <- p +
    ggplot2::geom_bar(stat = "identity") +
    my_theme() +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  p <- p +
    ggplot2::labs(y = "Percentage of Participants") +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                breaks = scales::pretty_breaks(n = 10))

  p <- p +
    ggplot2::labs(x = "Age Decade")
  p <- p +
    ggplot2::facet_grid(timeframe ~ .)

  p





  name <- "age_percentage"

  name <- paste0(name, "_last_month")

  name <- paste0(name, "_barplot.png")
  name <- paste0(dir_name, "/", name)

  ggplot2::ggsave(name, p, "png")

  if (upload != FALSE){
    dir_name <- paste0("nih_plots")
    zip_name <- paste0(dir_name, ".zip")
    utils::zip(zip_name, dir_name)
    googledrive::drive_upload(zip_name, path = googledrive::as_id(upload), overwrite = TRUE)
  }
}
