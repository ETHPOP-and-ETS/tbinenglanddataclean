#' A Function to Clean and Merge Observed and Projected Outmigration in England
#' 
#'  description This functions loads in observed and projected birth data produced by the Office
#'  of National Statistics and combines both datasets into a single tidy dataframe.
#'   and projected births can be downloaded 
#'   [here](https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea14principalprojectionenglandsummary).
#' @inheritParams clean_demographics_uk
#' @param birth_path The file path to the unformated observed births data, see the description for details.
#' @return A tidy data frame containing observed and projected births for England.
#' @export
#' @import magrittr, tidyr, ggplot2
#' @importFrom readxl read_excel cell_cols cell_rows
#' @importFrom dplyr rename select mutate bind_rows
#' @examples
#'
#'
clean_outmigration <- function(outmigration_path,
                               return = TRUE,
                               save = TRUE,
                               save_name = "uk_births",
                               save_path,
                               save_format = c("rds", "csv"),
                               theme_set = theme_minimal) {
  
  if (is.null(outmigration_path)) {
    stop("The path to the observed birth data must be specified")
  }
  
  n_toprow <- 6
  n_bottomrow <- 17
  
  migration_person <- read_excel(outmigration_path,
                                 sheet = "PERSONS",
                                 range = cell_rows(c(n_toprow,n_bottomrow)))
  migration_male <- read_excel(outmigration_path,
                               sheet = "MALES",
                               range = cell_rows(c(n_toprow,n_bottomrow)))
  migration_female <- read_excel(outmigration_path,
                                 sheet = "FEMALES",
                                 range = cell_rows(c(n_toprow,n_bottomrow)))
  
  row_names <- migration_person[, "...1"]
  migration_row <- which(row_names == "International migration outflows" &
                           !is.na(row_names))
  
  proj_migration <-
    rbind.data.frame(migration_person[migration_row, ],
                     migration_male[migration_row, ],
                     migration_female[migration_row, ]) %>% 
    mutate(sex = c("person", "male", "female")) %>% 
    select(-'...1', -'...2') %>%
    pivot_longer(cols = `2018`:`2117`,
                 names_to = "year",
                 values_to = "outflow") %>%
    mutate(outflow = as.numeric(outflow) * 1000,
           year = as.numeric(year))
  
  if (save) {
    save_data(proj_migration,
              name = save_name,
              path = save_path,
              format = save_format,
              message = "Demographic data saved to: ")
  }
  
  if (return) {
    return(proj_migration)
  }
}
