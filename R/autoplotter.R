#' Run the autoplotter
#' @importFrom magrittr %>%
run_autoplotter <- function() {
  mc_filtered <- mcdata::mc_download(datatype = "filtered") %>%
    mcdata::remove_svRT_outliers(.)

  mc_model_results <- mcdata::mc_model(mc_filtered)
  # Create directory for all of the plots
  dir_name <- paste0(Sys.Date(), "_mc_plots")
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }

  # Create Subfolders for each Plot type
  setwd(dir_name)
  if (!dir.exists("Demographics")) {
    dir.create("Demographics")
  }

  # Generate Plots
  ## Demographics
  setwd("Demographics")
  plot_demographic_barplot(mc_filtered, "alzheimer",
                           "First Degree Family History of Alzheimers")
  plot_demographic_barplot(mc_filtered, "diabetes", "Diabetes")
  plot_demographic_barplot(mc_filtered, "dizziness", "History of Dizziness")
  plot_demographic_barplot(mc_filtered, "hypertension", "Hypertension")
  plot_demographic_barplot(mc_filtered, "heart_disease", "Heart Disease")
  plot_demographic_barplot(mc_filtered, "smoking", "Smoking")
  plot_demographic_barplot(mc_filtered, "loss_of_consciousness",
                           "Loss of Conciousness")
  plot_demographic_barplot(mc_filtered, "drug_abuse", "Drug Abuse")
  plot_demographic_barplot(mc_filtered, "cancer", "Cancer")
  plot_demographic_barplot(mc_filtered, "stroke", "Stroke")
  plot_demographic_barplot(mc_filtered, "seizures", "Seizures")
  plot_demographic_barplot(mc_filtered, "education_attainment",
                           "Education Attainment")
  plot_demographic_barplot(mc_filtered, "sex", "Sex")
  plot_demographic_barplot(mc_filtered, "left_handed", "Left Handed")
  plot_demographic_barplot(mc_filtered, "hispanic_latino_or_spanish",
                           "Hispanic, Latino, or Spanish")
  plot_demographic_barplot(mc_filtered, "race", "Race")
  plot_demographic_barplot(mc_filtered, "race", "Race",
                           c("Black or African American", "Asian",
                             "Native Hawaiian or Other Pacific Islander",
                             "American Indian or Alaska Native", "Mixed"))

  setwd("../")
  ## Geographics
  if (!dir.exists("Geographics")) {
    dir.create("Geographics")
  }
  setwd("Geographics")
  plot_geo(mc_filtered, "world")
  plot_geo(mc_filtered, "us")

  setwd("../")
  ##Main Effects
  if (!dir.exists("Main_Effects")) {
    dir.create("Main_Effects")
  }
  setwd("Main_Effects")

  if (!dir.exists("PAL")) {
    dir.create("PAL")
  }
  setwd("PAL")
  if (!dir.exists("Linear")) {
    dir.create("Linear")
  }
  setwd("Linear")
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Sex", "sex",
                     c("Male", "Female"), model_results = mc_model_results)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Race",
                     "race", model_results = mc_model_results)
  plot_model_results(mc_filtered, "Age", model_results = mc_model_results)
  setwd("../")

  if (!dir.exists("Curvy")) {
    dir.create("Curvy")
  }
  setwd("Curvy")

  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Sex", "sex",
                     c("Male", "Female"), smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Race",
                     "race", smooth = TRUE,
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Age",
                     smooth = TRUE, model_results = mc_model_results)
  setwd("../")
  setwd("../")

  if (!dir.exists("SVRT")) {
    dir.create("SVRT")
  }
  setwd("SVRT")
  if (!dir.exists("Linear")) {
    dir.create("Linear")
  }
  setwd("Linear")
  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Sex",
                    "sex", c("Male", "Female"), game = "medianSVRT_no_outliers",
                    model_results = mc_model_results)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish",
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Race",
                     "race", game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Age",
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  setwd("../")

  if (!dir.exists("Curvy")) {
    dir.create("Curvy")
  }
  setwd("Curvy")

  plot_model_results(mc_filtered, "First Degree Family history of Alzheimer's",
                     "alzheimer", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Diabetes",
                     "diabetes", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Dizziness",
                     "dizziness", smooth = TRUE,
                     ame = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hypertension",
                     "hypertension", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Heart Disease",
                     "heart_disease", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Smoking",
                     "smoking", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Loss of Conciousness",
                     "loss_of_consciousness", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Drug Abuse",
                     "drug_abuse", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Cancer",
                     "cancer", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Stroke",
                     "stroke", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Seizures",
                     "seizures", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Education Attainment",
                     "education_attainment", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Sex",
                     "sex", c("Male", "Female"), smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Left Handed",
                     "left_handed", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Hispanic, Latino, or Spanish",
                     "hispanic_latino_or_spanish", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Race",
                     "race", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  plot_model_results(mc_filtered, "Age", smooth = TRUE,
                     game = "medianSVRT_no_outliers",
                     model_results = mc_model_results)
  setwd("../")
  setwd("../")
  setwd("../")
  mc_filtered_sex <- mc_filtered[mc_filtered$sex %in% c("Male", "Female"), ]
  # Animations

  if (!dir.exists("Animations")) {
    dir.create("Animations")
  }
  setwd("Animations")
  animate_error_bar(mc_filtered_sex, "sex", TRUE)
  animate_error_bar(mc_filtered, "education_attainment", TRUE)
}
