#' Combines ONS Demographic Data with LFS Data
#'
#' this is a rewrite of the original combine_ons_with_lfs()
#'
#' Takes demographic data summarised by \code{\link[tbinenglanddataclean]{clean_demographics_uk}} and
#' \code{\link[tbinenglanddataclean]{clean_labour_force_survey}} and combines it into a single tidy dataset.
#' Summary statistics and plots can be returned to check both datasets.
#' 
#' @inherit clean_demographics_uk
#' @param ons_name Character string of the file name of the ONS demographic data.
#' @param lfs_name Character string of the file name of the LFS demographic data.
#' @param countries A character string, the countries to include in the
#'                  dataset. By default only England is included. Note this is
#'                  reliant on the data being present in the demographic datasets.
#'
#' @return A tidy tibble of demographic data by age between 2000 and 2015 for the specified countries
#' for both ONS and LFS data.
#' @export
#' @importFrom dplyr mutate summarise select group_by mutate filter full_join bind_rows
#'
combine_ons_with_lfs2 <- function(data_path = "",
                                  ons_name = "E_demo_2000_2015.rds",
                                  lfs_name = "formatted_LFS_2000_2016.rds",
                                  countries = "England",
                                  rtn = TRUE,
                                  save = TRUE,
                                  save_name = "E_ons_lfs_2000_2016",
                                  save_path = "",
                                  save_format = "rds",
                                  verbose = TRUE) {
  
  demo_path <- file.path(data_path, ons_name)
  demo_2000_2015 <- readRDS(demo_path)
  
  if (verbose) {
    message("Loading demographic data from: ", demo_path)
  }
  
  lfs_path <- file.path(data_path, lfs_name)
  lfs_data <- readRDS(lfs_path)
  
  if (verbose) {
    message("Loading labour force survey data from: ", lfs_path)
  }
  
  # aggregate LFS to yearly population counts --------------------------
  # independent of sex
  # basically, marginalising weight over England and sex
  
  lfs_data_aggr <-
    lfs_data %>%
    filter(Country %in% countries) %>%
    group_by(Year, Age, CoB) %>%
    summarise(Population = sum(Weight)) %>%
    mutate(CoB = as.character(CoB))
  
  
  # Format demographics harmonise with lfs ---------------------------------
  
  demo_2000_2015 <- 
    demo_2000_2015 %>%
    mutate(CoB = 'Total') %>% # ie not broken down by UK born
    # mutate(CoB = CoB) %>% # WHAT IS THIS FOR?? NG
    mutate(Year = as.numeric(as.character(Year)))
  
  # row bind
  # because population won't match (?)
  demo_2000_2016_strat_est <-
    full_join(demo_2000_2015, lfs_data_aggr,
              by = c('Year', 'Age', 'CoB', 'Population')) %>%
    mutate(CoB = factor(CoB,
                        levels = c('Total', 'UK born', 'Non-UK born')))
  
  
  # Include comparison total from LFS -------------------------------------------
  
  demo_2000_2016_strat_est <-
    demo_2000_2016_strat_est %>%
    filter(!(CoB %in% 'Total')) %>%
    group_by(Age, Year) %>%
    summarise(Population = sum(Population)) %>%
    mutate(CoB = 'Total (LFS)') %>%
    bind_rows(demo_2000_2016_strat_est %>%
                mutate(CoB = as.character(CoB))) %>%
    mutate(CoB = factor(CoB,
                        levels = c('Total', 'Total (LFS)', 'UK born', 'Non-UK born')))
  
  
  # Add 5 year age groups ---------------------------------------------------
  demo_2000_2016_strat_est <- 
    demo_2000_2016_strat_est %>%
    mutate(`Age group` =
             as.character(Age) %>%
             replace(Age %in% '90+', '90') %>%
             as.numeric() %>%
             cut(breaks = seq(0,95,5),
                 right = FALSE,
                 ordered_result = TRUE,
                 labels = c(paste(seq(0,85,5), seq(4,89,5), sep = '-'), '90+')))
  
  ## Add 0-15, 16-65, 65+
  demo_2000_2016_strat_est <- 
    demo_2000_2016_strat_est %>%
    mutate(`Age group (condensed)` = Age %>%
             as.character %>%
             replace(Age %in% '90+', '90') %>%
             as.numeric() %>%
             cut(breaks = c(0, 15, 65, 91),
                 right = FALSE,
                 ordered_result = TRUE,
                 labels = c('0-14', '15-64', '65+'))) %>% 
    ungroup()
  
  if (save) {
    save_data(demo_2000_2016_strat_est,
              name = save_name,
              path = data_path,
              format = save_format,
              message = "ONS combined with LFS data saved to: ",
              verbose = verbose)
  }
  
  if (rtn) {
    return(demo_2000_2016_strat_est)
  }
}
