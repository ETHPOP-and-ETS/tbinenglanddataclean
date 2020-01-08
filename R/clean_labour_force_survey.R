#' Load and Clean Labour Force Survey Data
#'
#' @description This functions automatically detects Labour Force Survey data in the specified
#' directory and reads it into R. It then extracts the age, sex, UK birth status, and country.
#' From this it creates a tidy dataset. The data can be downloaded [here](https://discover.ukdataservice.ac.uk/catalogue/?sn=5461).
#' 
#' cry01: country of birth
#' pwtXX: sample weights
#' 
#' @inheritParams clean_demographics_uk
#' @param years A numeric vector specifying which years of data to clean
#' @param years_var A named list of character strings. Each character string should contain the variables
#' to extract from a given year and this should be named with the year of data to extract.
#' @return A tidy data frame of population broken down by country, age, sex and UK birth status
#' for 2000 to 2015.
#' @export
#' @importFrom dplyr mutate select rename
#' @importFrom haven read_stata
#' @importFrom purrr pmap
#' @import ggplot2
#' @examples
#'
clean_labour_force_survey <- function(data_path = "~/data/tb_data/LFS",
                                      years = 2000:2016,
                                      years_var = list('2000' = c('age', 'sex', 'cry', 'govtof', 'pwt07'),
                                                       '2001' = c('age', 'sex', 'cry01', 'country', 'phhwt07'), #, 'pwt07'),
                                                       '2002' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2003' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2004' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2005' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2006' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2007' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2008' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2009' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2010' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2011' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                                       '2012' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT14'),
                                                       '2013' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                                       '2014' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                                       '2015' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                                       '2016' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16')),
                                      rtn = TRUE,
                                      save = TRUE,
                                      save_name = "formatted_LFS_2000_2016",
                                      save_path = "~/data/tb_data/tbinenglanddataclean",
                                      save_format = "rds",
                                      verbose = TRUE,
                                      theme_set = theme_minimal) {
  
  # Read in LFS data --------------------------------------------------------
  ## data notes
  ## NA and DNA are coded as -8 and -9
  ## list data folders in directory
  LFS_folders <- list.files(path = data_path)
  
  ## ignore data
  LFS_folders <- LFS_folders[grep('rds', LFS_folders, invert = TRUE)]
  
  ## For each data folder in the directory
  LFS_data <-
    lapply(LFS_folders,
           function(x){
             
             ## Find path for folder
             folder_dir <- file.path(data_path, x)
             
             ## list data folder contents
             folder_folders <- list.files(path = folder_dir)
             
             ## find stata folder
             data_folder <- folder_folders[grep('stata', folder_folders)]
             
             ## find data folder dir
             data_sub_path <- file.path(folder_dir, data_folder)
             
             ## contents of data folder
             stata_folder <- list.files(path = data_sub_path)
             
             ## dat path
             full_path <- file.path(data_sub_path, stata_folder)
             
             if (verbose) {
               message("Data loaded from: ", full_path)
             }
             ## read in the data
             df <- read_stata(file = full_path, encoding = "latin1")
             
             return(df)
           })
  
  ## name data list
  names(LFS_data) <- LFS_folders
  
  # Extract key variables and combine ---------------------------------------
  
  form_LFS_data <-
    pmap(list(LFS_data, years, years_var),
         format_LFS()) %>%
    bind_rows() %>%
    mutate(Country = factor(Country),
           CoB = factor(CoB),
           Sex = factor(Sex)) %>%
    mutate(Age = replace(Age, Age >= 90, '90+')) %>%
    mutate(Age = factor(Age, levels = c(as.character(0:89), '90+')))
  
  
  if (verbose) {
    
    plot_LFS(form_LFS_data)
  }
  
  ## save formatted LFS data
  if (save) {
    save_data(form_LFS_data,
              name = save_name,
              path = save_path,
              format = save_format,
              message = "Cleaded LFS data has been saved to: ",
              verbose = verbose
    )
  }
  
  if(rtn) {
    return(form_LFS_data)
  }
}
