
#' format_LFS
#'
#' Different year surveys are slightly different
#' in terms of codes and groups so need to
#' treat separately.
#'
#' @param x LFS_data
#' @param year calendar year
#' @param year_var survey year specific codes
#'
#' @return
#' @export
#'
#' @examples
#' 
format_LFS2 <- function(x, year, year_var){
  
  x <-
    x %>%
    select_(.dots = year_var) %>%
    mutate(Year = year)
  
  if (year == 2000) {
    
    x <- format_LFS_2000(x, year)
    
  } else if (year == 2001) {
    
    x <- format_LFS_2001(x, year)
    
  } else if (year %in% 2002:2011) {
    
    x <- format_LFS_2002to2011(x, year)
    
  } else if (year %in% 2012:2016) {
    
    x <- format_LFS_2012to2016(x, year)
    
  } else {
    stop('Year has no defined variables,
         or cleaning process; check year_var')
  }
  
  return(x)
}