
#' format_LFS
#'
#' @param x 
#' @param year 
#' @param year_var 
#'
#' @return
#' @export
#'
#' @examples
format_LFS <- function(x, year, year_var){
  
  x <- x %>%
    select_(.dots = year_var) %>%
    mutate(Year = year)
  
  ## account for no avail of country
  if (year == 2000) {
    
    x %>%
      mutate(Age = replace(age, age %in% c(-8, -9), NA)) %>%
      select(-age) -> x
    
    ## country of residence
    x %>%
      mutate(Country = ifelse(govtof %in% c(1,2,3,4,5,6,7,8,9,10), 'England',
                              ifelse(govtof %in% c(11), 'Wales', ifelse(govtof %in% c(12), 'Scotland',
                                                                        ifelse(govtof %in% c(13), 'Wales', NA))))) %>%
      select(-govtof) -> x
    
    ## country of birth (UK/not UK)
    x %>%
      mutate(CoB = ifelse(cry %in% c(-8,-9), NA,
                          ifelse(cry %in% c(1), 'UK born', 'Non-UK born'))) %>%
      select(-cry) -> x
    
    ## formating of sex
    x %>%
      mutate(Sex = ifelse(sex %in% c(-8,-9), NA,
                          ifelse(sex %in% c(1), 'Male', 'Female'))) %>%
      select(-sex) -> x
    
    ## standardise weight
    x %>%
      rename(Weight = pwt07) -> x
    
  }else if (year == 2001) {
    
    x %>%
      mutate(Age = replace(age, age %in% c(-8, -9), NA)) %>%
      select(-age) -> x
    
    ## country of residence
    x %>%
      mutate(Country = 
               case_when(
                 country %in% c(-9, -8) ~ NA_character_,
                 country %in% c(1)   ~ 'England',
                 country %in% c(2)   ~ 'Wales',
                 country %in% c(3,4) ~ 'Scotland',
                 country %in% c(5)   ~ 'Northern Ireland',
                 TRUE ~ NA_character_)) %>%  
      select(-country) -> x
    
    ## country of birth (UK/not UK)
    x %>%
      mutate(CoB = ifelse(cry01 %in% c(-8,-9),
                          NA,
                          ifelse(cry01 %in% c(1,2,3,4,5),
                                 'UK born',
                                 'Non-UK born'))) %>%
      select(-cry01) -> x
    
    ## formating of sex
    x %>%
      mutate(Sex = ifelse(sex %in% c(-8,-9),
                          NA,
                          ifelse(sex %in% 1,
                                 'Male',
                                 'Female'))) %>%
      select(-sex) -> x
    
    ## standardise weight
    x %>%
      rename(Weight = pwt07) -> x
    
  }else if (year %in% 2002:2011) {
    ## clean age with R style missing
    x %>%
      mutate(Age = replace(AGE, AGE %in% c(-8, -9), NA)) %>%
      select(-AGE) -> x
    
    ## country of residence
    x %>%
      mutate(Country = ifelse(COUNTRY %in% c(-9, -8), NA,
                              ifelse(COUNTRY %in% c(1), 'England',
                                     ifelse(COUNTRY %in% c(2), 'Wales',
                                            ifelse(COUNTRY %in% c(3,4), 'Scotland', 'Northern Ireland'))))) %>%
      select(-COUNTRY) -> x
    
    ## Split due to  errors in variable encoding between 2002 and 2007
    if (year %in% c(2002:206)) {
      ## country of birth (UK/not UK)
      x %>%
        mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                            ifelse(CRY01 %in% c(1,2,3,4,5), 'UK born', 'Non-UK born'))) %>%
        select(-CRY01) -> x
    }else{
      x %>%
        mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                            ifelse(CRY01 %in% c(921, 922, 923, 924, 926), 'UK born', 'Non-UK born'))) %>%
        select(-CRY01) -> x
    }
    ## country of birth (UK/not UK)
    
    ## formating of sex
    x %>%
      mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                          ifelse(SEX %in% c(1), 'Male', 'Female'))) %>%
      select(-SEX) -> x
    
    ## standardise weight
    x %>%
      rename(Weight = PWT14) -> x
    
  }else if (year %in% 2012:2016) {
    ## clean age with R style missing
    x %>%
      mutate(Age = replace(AGE, AGE %in% c(-8, -9), NA)) %>%
      select(-AGE) -> x
    
    ## country of residence
    x %>%
      mutate(Country = ifelse(COUNTRY %in% c(-9, -8), NA,
                              ifelse(COUNTRY %in% c(1), 'England',
                                     ifelse(COUNTRY %in% c(2), 'Wales',
                                            ifelse(COUNTRY %in% c(3,4), 'Scotland', 'Northern Ireland'))))) %>%
      select(-COUNTRY) -> x
    ## country of birth (UK/not UK)
    x %>%
      mutate(CoB = ifelse(CRY12 %in% c(-8,-9), NA,
                          ifelse(CRY12 %in% c(921, 922, 923, 924, 926), 'UK born', 'Non-UK born'))) %>%
      select(-CRY12) -> x
    
    ## formating of sex
    x %>%
      mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                          ifelse(SEX %in% c(1), 'Male', 'Female'))) %>%
      select(-SEX) -> x
    
    #Set weights based on the variable
    if (year %in% c(2012)) {
      ## standardise weight
      x %>%
        rename(Weight = PWT14) -> x
    }else{
      ## standardise weight
      x %>%
        rename(Weight = PWT16) -> x
    }
    
  }else{
    stop('Year has no defined variables, or cleaning process; check year_var')
  }
  
  return(x)
}