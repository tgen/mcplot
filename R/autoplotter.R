#' Run the autoplotter
#' @param prod (optional) Default = FALSE. Indicates if this is running on production
#' @param geo (optional) Default = TRUE. Set to FALSE to remove sf dependency
#' @param upload (optional) Default = FALSE. To upload output to drive, set this equal to drive folder ID
#' @importFrom magrittr %>%
#' @export
run_autoplotter <- function(prod = FALSE, geo = TRUE,
                            upload = FALSE) {
  if (prod == FALSE){
    mc_filtered <- mcdata::mc_download(datatype = "filtered") %>%
      mcdata::remove_svRT_outliers(.)
  } else {
    mc_filtered <- mcdashboard::importData(prod = TRUE) %>%
      mcdata::mc_filter(.) %>%
      mcdata::remove_svRT_outliers(.)
  }


  mc_model_results <- mcdata::mc_model(mc_filtered)
  # Create directory for all of the plots
  dir_name <- paste0("mc_plots")
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }

  # Create Subfolders for each Plot type
  if (!dir.exists(paste0("./",dir_name, "/Demographics"))) {
    dir.create(paste0("./",dir_name, "/Demographics"))
  }

  demo_path <- paste0("./",dir_name,"/Demographics/")
  # Generate Plots
  ## Demographics
  plot_demographic_barplot(mc_filtered, "alzheimer",
                           "First Degree Family History of Alzheimers",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "diabetes", "Diabetes",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "dizziness", "History of Dizziness",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "hypertension", "Hypertension",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "heart_disease", "Heart Disease",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "smoking", "Smoking",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "loss_of_consciousness",
                           "Loss of Conciousness",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "drug_abuse", "Drug Abuse",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "cancer", "Cancer",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "stroke", "Stroke",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "seizures", "Seizures",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "education_attainment",
                           "Education Attainment",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "sex", "Sex",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "left_handed", "Left Handed",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "hispanic_latino_or_spanish",
                           "Hispanic, Latino, or Spanish",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "race", "Race",
                           path = demo_path)
  plot_demographic_barplot(mc_filtered, "race", "Race",
                           c("Black or African American", "Asian",
                             "Native Hawaiian or Other Pacific Islander",
                             "American Indian or Alaska Native", "Mixed"),
                           path = demo_path)

  ## Geographics
  if (geo == TRUE) {
    if (!dir.exists(paste0("./",dir_name, "/Geographics"))) {
      dir.create(paste0("./",dir_name, "/Geographics"))
    }
    geo_path <- paste0("./",dir_name, "/Geographics/")
    plot_geo(mc_filtered, "world", path = geo_path)
    plot_geo(mc_filtered, "us", path = geo_path)
  }

  ##Main Effects
  if (!dir.exists(paste0("./",dir_name, "/Main_Effects"))) {
    dir.create(paste0("./",dir_name, "/Main_Effects"))
  }
  main_effects_path <- paste0("./",dir_name, "/Main_Effects/")
  if (!dir.exists(paste0(main_effects_path, "PAL"))) {
    dir.create(paste0(main_effects_path, "PAL"))
  }
  pal_path <- paste0(main_effects_path, "PAL/")
  if (!dir.exists(paste0(pal_path, "Linear"))) {
    dir.create(paste0(pal_path, "Linear"))
  }
  pal_linear_path <- paste0(pal_path, "Linear/")
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Sex", "sex",
                     c("Male", "Female"), model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish",
                     model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Race",
                     "race", model_results = mc_model_results,
                     path = pal_linear_path)
  plot_model_results(mc_filtered, "Age", model_results = mc_model_results,
                     path = pal_linear_path)

  if (!dir.exists(paste0(pal_path,"Curvy"))) {
    dir.create(paste0(pal_path,"Curvy"))
  }
  pal_curvy_path <- paste0(pal_path,"Curvy/")
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Sex", "sex",
                     c("Male", "Female"), smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Race",
                     "race", smooth = TRUE,
                     model_results = mc_model_results,
                     path = pal_curvy_path)
  plot_model_results(mc_filtered, "Age",
                     smooth = TRUE, model_results = mc_model_results,
                     path = pal_curvy_path)

  svrt_path <- paste0(main_effects_path, "SVRT/")
  if (!dir.exists(svrt_path)) {
    dir.create(svrt_path)
  }
  svrt_linear_path <- paste0(svrt_path, "Linear/")
  if (!dir.exists(svrt_linear_path)) {
    dir.create(svrt_linear_path)
  }
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Sex",
                    "sex", c("Male", "Female"), game = "medianSVRT_no_outliers",
                    model_results = mc_model_results,
                    path = svrt_linear_path)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish",
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Race",
                     "race", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  plot_model_results(mc_filtered, "Age",
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_linear_path)
  svrt_curvy_path <- paste0(svrt_path, "Curvy/")
  if (!dir.exists(svrt_curvy_path)) {
    dir.create(svrt_curvy_path)
  }
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Sex",
                     "sex", c("Male", "Female"), smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Race",
                     "race", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  plot_model_results(mc_filtered, "Age", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results,
                     path = svrt_curvy_path)
  mc_filtered_sex <- mc_filtered[mc_filtered$sex %in% c("Male", "Female"), ]
  # Animations
  if (geo == TRUE) {
    animations_path <- paste0("./",dir_name, "/Animations/")
    if (!dir.exists(animations_path)) {
      dir.create(animations_path)
    }
    animate_error_bar(mc_filtered_sex, "sex", TRUE, path = animations_path)
    animate_error_bar(mc_filtered, "education_attainment", TRUE,
                      path = animations_path)
  }

  #Upload to drive
  if (upload != FALSE){
    dir_name <- "mc_plots"
    zip_name <- paste0(dir_name, ".zip")
    utils::zip(zip_name, dir_name)
    googledrive::drive_upload(zip_name, path = googledrive::as_id(upload), overwrite = TRUE)
  }
}

#' Run monthly NIH graphs
#' @importFrom magrittr %>%
#' @export
run_monthly_graphs <- function(mc = NULL) {
  if (is.null(mc)){
    mc <- mcdata::mc_download("tidy")
  }
  # Subset by months
  mc_last_month <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]
  mc_last_month <- mc[as.Date(mc[["created_at"]]) > lubridate::floor_date(Sys.Date(), "month") - months(1), ]

  for (x in unique(mc_last_month[!is.na(mc_last_month[["race"]]),][["race"]])){
    plot_demographic_barplot(mc_last_month, "race", subset = x, age_decade = TRUE, percentage = FALSE)
    plot_demographic_barplot(mc_last_month, "race", subset = x, age_decade = TRUE)
  }
  plot_age_barplot(mc_last_month, percentage = TRUE, age_decade = TRUE)
  plot_age_barplot(mc_last_month, percentage = FALSE, age_decade = TRUE)
  get_race_ethnicity_demographics <- function(mc){
    mc <- mc[!is.na(mc[["race"]]), ]
    demographics <- mcdata::report_demographics(mcs = list(mc[mc[["race"]] == "White", ],
                                                           mc[mc[["race"]] == "Native Hawaiian or Other Pacific Islander", ],
                                                           mc[mc[["race"]] == "Mixed", ],
                                                           mc[mc[["race"]] == "Asian", ],
                                                           mc[mc[["race"]] == "Black or African American", ],
                                                           mc[mc[["race"]] == "American Indian or Alaska Native", ],
                                                           mc[mc[["hispanic_latino_or_spanish"]] == TRUE, ],
                                                           mc),
                                                          labels = list("White",
                                                                        "Native Hawaiian or Other Pacific Islander",
                                                                        "Mixed",
                                                                        "Asian",
                                                                        "Black or African American",
                                                                        "American Indian or Alaska Native",
                                                                        "Hispanic Latino or Spanish",
                                                                        "total")
                                                          )

    filtered_demographics <- demographics[demographics[["demographic"]] %in%
                                            c("sex", "hispanic_latino_or_spanish",
                                              "age_decade", "race",
                                              "total"), ]
    filtered_demographics <- filtered_demographics[!(filtered_demographics[["value"]] %in%
                                                       c("Male to female", "Female to male",
                                                         "unknown")), ]
    return(filtered_demographics)
  }
  demographics_all_time <- get_race_ethnicity_demographics(mc)

  mc_last_month <- mc[as.Date(mc[["created_at"]]) < lubridate::floor_date(Sys.Date(), "month"), ]
  mc_last_month <- mc_last_month[as.Date(mc_last_month[["created_at"]]) > lubridate::floor_date(Sys.Date(), "month") - months(1), ]

  demographics_last_month <- get_race_ethnicity_demographics(mc_last_month)

  xlsx::write.xlsx(demographics_all_time,
                   file=paste0(Sys.Date(),"race_ethnicity_demographics.xlsx"),
                   sheetName="All-Time", row.names=FALSE)
  xlsx::write.xlsx(demographics_last_month,
                   file=paste0(Sys.Date(),"race_ethnicity_demographics.xlsx"),
                   sheetName="Last-Month", append = TRUE,row.names=FALSE)

}
