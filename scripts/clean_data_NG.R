#
# run same cleaning script
# as in vignette
# but on more recent data
# and local system
#
# Nathan Green


library(purrr)
library(dplyr)
devtools::load_all(".")

run_envir <- "D:/data"

# output save here: data/E_demo_2000_2015
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

# output save here: data/formatted_LFS_2000_2016
clean_labour_force_survey(years = 2015:2016,
                          rtn = FALSE,
                          save = TRUE,
                          save_name = "formatted_LFS_2015_2016",
                          save_format = c("rds", "csv"),
                          save_path = run_envir,
                          verbose = TRUE)

combine_ons_with_lfs2(data_path = run_envir,
                      ons_name = "E_demo_2000_2015.rds",
                      lfs_name = "formatted_LFS_2015_2016.rds",
                      countries = "England",
                      rtn = FALSE,
                      save = TRUE,
                      save_name = "E_ons_lfs_2000_2016",
                      save_path = run_envir,
                      save_format = c("rds", "csv"),
                      verbose = TRUE) 

##TODO:
clean_and_munge_england_births(birth_path = paste0(run_envir, "/data/tb_data/UK_demographics/annual_reference_table.xls"),
                               projected_birth_path = paste0(run_envir, "/data/tb_data/UK_demographics/england_population_projections.xls"),
                               return = FALSE,
                               save = TRUE,
                               save_name = "england_births",
                               save_path = paste0(run_envir, "/data/tb_data/tbinenglanddataclean"),
                               save_format = c("rds", "csv"),
                               verbose = TRUE)