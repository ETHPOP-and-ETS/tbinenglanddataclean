
run_envir <- "D:/data"

clean_demographics_uk(data_path = paste0(run_envir, "/ONS"),
                      demo_2000 = NA,
                      demo_2001_2015 = "MYEB1_detailed_population_estimates_series_UK_(2018).csv",
                      countries = c("E"),
                      rtn = FALSE,
                      save = TRUE,
                      save_name = "E_demo_2000_2015",
                      save_path = run_envir,
                      save_format = c("rds", "csv"),
                      verbose = TRUE)

library(purrr)
library(dplyr)
clean_labour_force_survey(years = 2000:2001,
                          rtn = FALSE,
                          save = TRUE,
                          save_name = "formatted_LFS_2000_2016",
                          save_format = c("rds", "csv"),
                          verbose = TRUE)


combine_ons_with_lfs(data_path = paste0(run_envir, "/data/tb_data/tbinenglanddataclean"),
                     ons_name = "E_demo_2000_2015.rds",
                     lfs_name = "formatted_LFS_2000_2016.rds",
                     countries = "England",
                     rtn = FALSE,
                     save = TRUE,
                     save_name = "E_ons_lfs_2000_2016",
                     save_path = paste0(run_envir, "/data/tb_data/tbinenglanddataclean"),
                     save_format = c("rds", "csv"),
                     verbose = TRUE) 