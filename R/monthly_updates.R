#' Participation N Line
#'
#' @param mc_tidy mcdata object
plot_participation_line <- function(mc_tidy){
  mc_tidy_barplot <- dplyr::distinct(mc_tidy,user_id, .keep_all= TRUE)
  mc_tidy_barplot[["month"]] <- lubridate::floor_date(mc_tidy_barplot[["created_at"]],unit = "month")
  mc_tidy_barplot<- mc_tidy_barplot[as.Date(mc_tidy_barplot[["created_at"]]) >= as.Date("2021-10-01"), ]
  mc_tidy_barplot<- mc_tidy_barplot[as.Date(mc_tidy_barplot[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]

  my_table <- as.data.frame(table(mc_tidy_barplot[["month"]]))

  #Get mc totals for each month in barplot
  baseline_barplot <- sum(as.Date(mc_tidy[["created_at"]]) < as.Date("2021-10-01"), na.rm = TRUE)
  my_table[["total"]] <- format(baseline_barplot + cumsum(my_table[["Freq"]]), big.mark = ",")
  my_table[["Var1"]] <- as.Date(my_table[["Var1"]])

  p<- ggplot2::ggplot(data=my_table, ggplot2::aes(x=Var1, y=Freq, group = 1, label = total)) +
    ggplot2::geom_line()+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_text(hjust=0.5, vjust=-1)+
    ggplot2::theme(text = ggplot2::element_text(size = 15))+
    modified_theme()+
    ggplot2::labs(x = "Month", y = "Number of Participants Recruited (Monthly)")+
    ggplot2::scale_y_continuous(breaks = seq(0, 5500, 500), limits = c(0, 5500))+
    ggplot2::scale_x_date(date_labels = "%b %Y")
  ggplot2::ggsave(paste0(Sys.Date(), "_monthly_participation_lineplot.png"), p, width = 8, height = 5)
  p
}


#' Recruitment Projection Line
#'
#' @param mc mcdata object
#' @param projection_start_date (optional) Default = "2020-01-01", Start date of projection
#' @param end_date (optional) Default = "2030-01-01", End date of projection
plot_projected_participants <- function(mc, projection_start_date = "2020-01-01",
                                        end_date = "2030-01-01",
                                        goal = FALSE){
  mc <- dplyr::distinct(mc, user_id, .keep_all = TRUE)
  baseline_count <- sum(as.Date(mc[["created_at"]]) < as.Date(projection_start_date), na.rm = TRUE)

  mc_projection <- mc
  mc_projection[["month"]] <- lubridate::floor_date(mc_projection[["created_at"]],unit = "month")
  mc_projection<- mc_projection[as.Date(mc_projection[["created_at"]]) >= as.Date(projection_start_date), ]
  mc_projection<- mc_projection[as.Date(mc_projection[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]

  my_table <- as.data.frame(table(mc_projection[["month"]]))
  six_mos_table <- my_table[(nrow(my_table)-6):nrow(my_table),]
  average_rate<- mean(six_mos_table[["Freq"]])

  line_values <- data.frame(Month = as.character(my_table[["Var1"]]), Total = cumsum(my_table[["Freq"]]), Record = "Past Data", stringsAsFactors=FALSE)
  start_date<- as.Date(line_values[["Month"]][[length(line_values[["Month"]])]])

  x=lubridate::interval(lubridate::ymd(start_date),
                        lubridate::ymd(end_date))
  x= x %/% months(1)
  test <- start_date + months(1:x)
  test_values <- line_values[["Total"]][[length(line_values[["Total"]])]] + (average_rate * 1:x)
  df<- data.frame(Month = as.character(test), Total = test_values, Record = "Projected", stringsAsFactors=FALSE)
  rm(test)
  rm(test_values)
  final_line_values <- rbind(line_values, df)
  final_line_values[["Total"]] <- final_line_values[["Total"]] + baseline_count
  final_line_values[["Month"]] <- as.Date(final_line_values[["Month"]])

  p <- ggplot2::ggplot(data=final_line_values,
                       ggplot2::aes(x=Month, y=Total, group=1, color = Record)) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    modified_theme()+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0))+
    ggplot2::scale_x_date(date_breaks = "1 year",
                          date_minor_breaks = "1 month",
                          date_labels = "%Y")+
    ggplot2::scale_y_continuous(label = scales::comma)+
    ggplot2::labs(x = "Year", y = "Total Participants")
  ggplot2::ggsave(paste0(Sys.Date(), "_monthly_overall_participation_projection.png"))
  p
}


#' Monthly demographic Table
#'
#' @param mc_tidy mcdata object
#' @param current_month (optional) Default = Current month. The later month used for comparison
#' @param last_month (optional) Default = Last month. The earlier month used for comparison
#' @importFrom lubridate %within%
monthly_demographic_comparison <- function(mc_tidy,
                                           current_month = lubridate::floor_date(Sys.Date(), "month"),
                                           last_month = lubridate::floor_date(Sys.Date(), "month") - months(1)) {
  month1 <- lubridate::floor_date(last_month, "month")
  month2 <- lubridate::floor_date(current_month, "month")

  mc_last_month<- mc_tidy[mc_tidy[["created_at"]] %within%
                      lubridate::interval(start = lubridate::ymd(month1),
                      lubridate::ymd(month1)+months(1)-lubridate::days(1)), ]


  mc_this_month<- mc_tidy[mc_tidy[["created_at"]] %within% lubridate::interval(start = lubridate::ymd(month2),
                                                                    lubridate::ymd(month2)+months(1)-lubridate::days(1)), ]

  demographics <- mcdata::report_demographics(mcs = list(mc_last_month, mc_this_month),
                                     labels = list(month1, month2))
  demographics[["Delta"]] <- round(as.numeric(gsub("%", "",demographics[[paste0(month2, "_prop")]]))-as.numeric(gsub("%", "",demographics[[paste0(month1, "_prop")]])),2)
  demographics[["Delta"]] <- paste0(as.character(demographics[["Delta"]]),"%")

  write.csv(demographics,paste0(month1, "_",month2,"_demographics.csv"), row.names = FALSE)
}
