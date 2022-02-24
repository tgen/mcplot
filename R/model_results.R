#' Split model results labels for plotting
#'
#' @param model mcmodel results
split_demo <- function(model) {
  model["Demographic"] <- NA
  model["Subset"] <- NA
  model[model["term"] == "age", ]["Demographic"] <- "age"
  model[model["term"] == "age", ]["Subset"] <- "age"
  model[model["term"] == "sexMale", ]["Demographic"] <- "sex"
  model[model["term"] == "sexMale", ]["Subset"] <- "Male"
  model[model["term"] == "sexMale to female", ]["Demographic"] <- "sex"
  model[model["term"] == "sexMale to female", ]["Subset"] <- "Male to female"
  model[model["term"] == "sexFemale to male", ]["Demographic"] <- "sex"
  model[model["term"] == "sexFemale to male", ]["Subset"] <- "Female to male"
  model[model["term"] == "education_attainment", ]["Demographic"] <- "education_attainment"
  model[model["term"] == "education_attainment", ]["Subset"] <- "education_attainment"
  model[model["term"] == "raceAmerican Indian or Alaska Native", ]["Demographic"] <- "race"
  model[model["term"] == "raceAmerican Indian or Alaska Native", ]["Subset"] <- "American Indian or Alaska Native"
  model[model["term"] == "raceAsian", ]["Demographic"] <- "race"
  model[model["term"] == "raceAsian", ]["Subset"] <- "Asian"
  model[model["term"] == "raceBlack or African American", ]["Demographic"] <- "race"
  model[model["term"] == "raceBlack or African American", ]["Subset"] <- "Black or African American"
  model[model["term"] == "raceMixed", ]["Demographic"] <- "race"
  model[model["term"] == "raceMixed", ]["Subset"] <- "Mixed"
  model[model["term"] == "raceNative Hawaiian or Other Pacific Islander", ]["Demographic"] <- "race"
  model[model["term"] == "raceNative Hawaiian or Other Pacific Islander", ]["Subset"] <- "Native Hawaiian or Other Pacific Islander"
  model[model["term"] == "number_of_daily_medications", ]["Demographic"] <- "number_of_daily_medications"
  model[model["term"] == "number_of_daily_medications", ]["Subset"] <- "number_of_daily_medications"
  model[model["term"] == "left_handedTRUE", ]["Demographic"] <- "left_handed"
  model[model["term"] == "left_handedTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "hispanic_latino_or_spanishTRUE", ]["Demographic"] <- "hispanic_latino_or_spanish"
  model[model["term"] == "hispanic_latino_or_spanishTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "loss_of_consciousnessTRUE", ]["Demographic"] <- "loss_of_consciousness"
  model[model["term"] == "loss_of_consciousnessTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "seizuresTRUE", ]["Demographic"] <- "seizures"
  model[model["term"] == "seizuresTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "dizzinessTRUE", ]["Demographic"] <- "dizziness"
  model[model["term"] == "dizzinessTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "smokingTRUE", ]["Demographic"] <- "smoking"
  model[model["term"] == "smokingTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "alzheimerTRUE", ]["Demographic"] <- "alzheimer"
  model[model["term"] == "alzheimerTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "marital_statusmarried", ]["Demographic"] <- "marital_status"
  model[model["term"] == "marital_statusmarried", ]["Subset"] <- "married"
  model[model["term"] == "marital_statuswidowed", ]["Demographic"] <- "marital_status"
  model[model["term"] == "marital_statuswidowed", ]["Subset"] <- "widowed"
  model[model["term"] == "marital_statusunreported", ]["Demographic"] <- "marital_status"
  model[model["term"] == "marital_statusunreported", ]["Subset"] <- "unreported"
  model[model["term"] == "diabetesTRUE", ]["Demographic"] <- "diabetes"
  model[model["term"] == "diabetesTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "hypertensionTRUE", ]["Demographic"] <- "hypertension"
  model[model["term"] == "hypertensionTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "heart_diseaseTRUE", ]["Demographic"] <- "heart_disease"
  model[model["term"] == "heart_diseaseTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "cancerTRUE", ]["Demographic"] <- "cancer"
  model[model["term"] == "cancerTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "brain_diseaseTRUE", ]["Demographic"] <- "brain_disease"
  model[model["term"] == "brain_diseaseTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "strokeTRUE", ]["Demographic"] <- "stroke"
  model[model["term"] == "strokeTRUE", ]["Subset"] <- "TRUE"
  model[model["term"] == "drug_abuseTRUE", ]["Demographic"] <- "drug_abuse"
  model[model["term"] == "drug_abuseTRUE", ]["Subset"] <- "TRUE"
  model[["Legend"]] <- paste0(model[["Subset"]], "\np: ", formatC(model[["Pr(>|t|)"]], format = "e", digits = 2), "\nEffect Size: ",
                             format(model[["Estimate"]], digits = 1))
  model
}


#' Plot model results line plots by demographic
#'
#' @param mc An mc object from the mcdata package.
#' @param plain_title (optional) Title for plot. Default = ""
#' @param demographic (optional) Demographic to plot. Default = "age"
#' @param demographic_subset (optional) A vector containing the demographic values you wish to plot
#' @param smooth (optional) Default = FALSE, returning linear lines. If TRUE, smooth lines are returned
#' @param game (optional) Default = "totalcorrect". For SVRT results, use "medianSVRT_no_outliers"
#' @param model_results  (optional) mc_model output object
plot_model_results <- function(mc, plain_title = "", demographic = "age",
                                     demographic_subset = FALSE, smooth = FALSE,
                                     game = "totalcorrect",
                                     model_results = NA) {
  if (is.na(model_results)) {
    model_results <- mcdata::mc_model(mc_filtered)
  }
  if (game == "medianSVRT_no_outliers") {
    model <- model_results[model_results[["dependent_variable"]] == "attnRT_median", ][["coefficients"]][[1]] %>%
      mcplot::split_demo() %>%
      tidyr::drop_na()
  } else {
    model <- model_results[model_results[["dependent_variable"]] == game, ][["coefficients"]][[1]] %>%
      mcplot::split_demo() %>%
      tidyr::drop_na()
  }
  title <- paste0(plain_title, " (p: ", formatC(model[model[["Demographic"]] == as.character(demographic), ][["Pr(>|t|)"]], format = "e", digits = 2),
                  ", Effect Size: ", format(model[model[["Demographic"]] == as.character(demographic), ][["Estimate"]], digits = 1), ")")
  mc <- mc[mc[["age"]] <= 90, ]
  if (demographic == "age") {
    temp <- data.frame(totalcorrect = mc[[game]], age = mc[["age"]])
    p <- ggplot2::ggplot(temp, ggplot2::aes(x = age, y = totalcorrect)) +
      my_theme() +
      ggplot2::labs(x = "Age", y = "Score", title = title)
    ifelse(smooth == TRUE,
           p <- p + ggplot2::geom_smooth(),
           p <- p + ggplot2::geom_smooth(method = lm))
  } else {
    temp <- data.frame(totalcorrect = mc[[game]], line_selection = mc[[demographic]], age = mc[["age"]])
    temp <- temp[!is.na(temp$line_selection), ]
    if (demographic_subset != FALSE) {
      temp <- temp[temp[["line_selection"]] %in% demographic_subset, ]
    }
    temp[["line_selection"]] <- as.character(temp[["line_selection"]])
    if (c(TRUE, FALSE) %in% unique(mc[[demographic]]) || "Male" %in% unique(mc[[demographic]])) {
      temp[["Legend"]] <- temp[["line_selection"]]
    } else {
      temp <- merge(temp,
                    model[model[["Demographic"]] == as.character(demographic), ],
                    by.x = "line_selection", by.y = "Subset", all = T)
      title <- plain_title
      temp[is.na(temp["Legend"]), ]["Legend"] <- temp[is.na(temp["Legend"]), ]["line_selection"]
    }
    p <- ggplot2::ggplot(temp, ggplot2::aes(x = age, y = totalcorrect, col = Legend)) +
      my_theme() +
      ggplot2::labs(x = "Age", y = "Score", title = title)
    ifelse(smooth == TRUE,
           p <- p + ggplot2::geom_smooth(ggplot2::aes(group = Legend)),
           p <- p + ggplot2::geom_smooth(ggplot2::aes(group = Legend), method = lm))
  }
  p <- p +
    ggplot2::scale_x_continuous(breaks = seq(from = 20, to = 100, by = 5)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ggplot2::guides(col = ggplot2::guide_legend(byrow = TRUE)) +
    ggplot2::theme(legend.spacing.y = ggplot2::unit(0.2, "cm")) +
    ggplot2::scale_fill_manual(values = mcdata::mc_palette()) +
    ggplot2::scale_color_manual(values = mcdata::mc_palette())
  name <- paste0(demographic, "_line_plot.png")
  ggplot2::ggsave(name, p, "png")
  return(p)
}
