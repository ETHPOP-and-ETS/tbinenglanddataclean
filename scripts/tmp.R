
run_envir <- "D:/data"

clean_demographics_uk(data_path = paste0(run_envir, "/ONS"),
                      demo_2000 = NA,
                      demo_2001_2015 = "MYEB1_detailed_population_estimates_series_UK_(2018).csv",
                      countries = c("E"),
                      return = FALSE,
                      save = TRUE,
                      save_name = "E_demo_2000_2015",
                      save_path = run_envir,
                      save_format = c("rds", "csv"),
                      verbose = TRUE)
