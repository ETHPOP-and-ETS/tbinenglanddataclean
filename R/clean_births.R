
#' A Function to Clean and Merge Observed and Projected Births in England
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
clean_births <- function(birth_path,
                         return = TRUE,
                         save = TRUE,
                         save_name = "uk_births",
                         save_path,
                         save_format = c("rds", "csv"),
                         theme_set = theme_minimal) {
  
  if (is.null(birth_path)) {
    stop("The path to the observed birth data must be specified")
  }
  
  births_person <- read_excel(birth_path,
                              sheet = "PERSONS",
                              range = cell_rows(c(6,10)))
  births_male <- read_excel(birth_path,
                            sheet = "MALES",
                            range = cell_rows(c(6,10)))
  births_female <- read_excel(birth_path,
                              sheet = "FEMALES",
                              range = cell_rows(c(6,10)))
  
  # birth value row
  row_names <- births_person[, "...1"]
  birth_row <- which(row_names == "Births" &
                       !is.na(row_names))
  
  ## clean proj births - removing years that are present in the birth data
  proj_births <-
    rbind.data.frame(births_person[birth_row, ],
                     births_male[birth_row, ],
                     births_female[birth_row, ]) %>% 
    mutate(sex = c("person", "male", "female")) %>% 
    select(-'...1', -'...2') %>%
    pivot_longer(cols = `2018`:`2117`,
                 names_to = "year",
                 values_to = "births") %>%
    mutate(births = as.numeric(births) * 1000,
           year = as.numeric(year))
  
  if (save) {
    save_data(proj_births,
              name = save_name,
              path = save_path,
              format = save_format,
              message = "Demographic data saved to: ",
              verbose = verbose)
  }
  
  if (return) {
    return(births)
  }
}
