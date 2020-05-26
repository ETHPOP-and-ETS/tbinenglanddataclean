#
format_LFS_2000 <- function(x, year) {
  
  x %>%
    mutate(Age = replace(age,
                         age %in% c(-8,-9), NA)) %>%
    select(-age) -> x
  
  ## country of residence
  x <- 
    x %>%
    mutate(Country = 
             case_when(
               govtof %in% 1:10 ~ 'England',
               govtof %in% 11   ~ 'Wales',
               govtof %in% 12   ~ 'Scotland',
               govtof %in% 13   ~ 'Northern Ireland',
               TRUE ~ NA_character_)) %>%  
    select(-govtof)
  
  ## country of birth (UK/not UK)
  x %>%
    mutate(CoB = ifelse(cry %in% c(-8,-9), NA,
                        ifelse(cry == 1,
                               'UK born', 'Non-UK born'))) %>%
    select(-cry) -> x
  
  ## formatting of sex
  x %>%
    mutate(Sex = ifelse(sex %in% c(-8,-9), NA,
                        ifelse(sex == 1,
                               'Male', 'Female'))) %>%
    select(-sex) -> x
  
  ## standardise weight
  x %>%
    rename(Weight = pwt07) -> x
  x
}

#
format_LFS_2001 <- function(x, year) {
  
  x %>%
    mutate(Age = replace(age, age %in% c(-8,-9), NA)) %>%
    select(-age) -> x
  
  ## country of residence
  x %>%
    mutate(Country = 
             case_when(
               country %in% c(-9,-8) ~ NA_character_,
               country %in% 1        ~ 'England',
               country %in% 2        ~ 'Wales',
               country %in% c(3,4)   ~ 'Scotland',
               country %in% 5        ~ 'Northern Ireland',
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
  
  ## formatting of sex
  x %>%
    mutate(Sex = ifelse(sex %in% c(-8,-9),
                        NA,
                        ifelse(sex == 1,
                               'Male',
                               'Female'))) %>%
    select(-sex) -> x
  
  ## standardise weight
  x %>%
    rename(Weight = pwt07) -> x

  x
}

#
format_LFS_2002to2011 <- function(x, year) {
  
  ## clean age with R style missing
  x %>%
    mutate(Age = replace(AGE, AGE %in% c(-8,-9), NA)) %>%
    select(-AGE) -> x
  
  ## country of residence
  x %>%
    mutate(Country = 
             case_when(
               COUNTRY %in% c(-9, -8) ~ NA_character_,
               COUNTRY %in% c(1)      ~ 'England',
               COUNTRY %in% c(2)      ~ 'Wales',
               COUNTRY %in% c(3,4)    ~ 'Scotland',
               COUNTRY %in% c(5)      ~ 'Northern Ireland',
               TRUE ~ NA_character_)) %>%  
    select(-COUNTRY) -> x
  
  ## country of birth (UK/not UK)
  ## Split due to  errors in variable encoding between 2002 and 2007
  if (year %in% c(2002:2006)) {
    ## country of birth (UK/not UK)
    x %>%
      mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                          ifelse(CRY01 %in% c(1,2,3,4,5),
                                 'UK born', 'Non-UK born'))) %>%
      select(-CRY01) -> x
  }else{
    UK_codes <- c(921, 922, 923, 924, 926)
    x %>%
      mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                          ifelse(CRY01 %in% UK_codes,
                                 'UK born', 'Non-UK born'))) %>%
      select(-CRY01) -> x
  }
  
  if (year == 2010) {
    
    ## format ethnicity
    x %>%
      mutate(ethgrp = 
               case_when(
                 ETHCEN15 %in% c(-9,-8)    ~ NA_character_,
                 ETHCEN15 %in% c(-6,1,2)   ~ 'White',
                 ETHCEN15 %in% c(3,4,5,6)  ~ 'Mixed',
                 ETHCEN15 %in% 7           ~ 'Indian',
                 ETHCEN15 %in% 8           ~ 'Pakistani',
                 ETHCEN15 %in% 9           ~ 'Bangladeshi',
                 ETHCEN15 %in% c(10,14)    ~ 'Asian',
                 ETHCEN15 %in% c(11,12,13) ~ 'Black/Black British',
                 ETHCEN15 %in% 15          ~ 'Other',
                 TRUE ~ NA_character_)) %>%  
      select(-ETHCEN15) -> x
    
  }
  
  if (year == 2011) {
    
    ## format ethnicity
    x %>%
      mutate(ethgrp = 
               case_when(
                 ETHGBEUL %in% c(-9,-8) ~ NA_character_,
                 ETHGBEUL %in% c(1,2,3) ~ 'White',
                 ETHGBEUL %in% 4        ~ 'Mixed',
                 ETHGBEUL %in% 5        ~ 'Indian',
                 ETHGBEUL %in% 6        ~ 'Pakistani',
                 ETHGBEUL %in% 7        ~ 'Bangladeshi',
                 ETHGBEUL %in% c(8,9)   ~ 'Asian',
                 ETHGBEUL %in% 10       ~ 'Black/Black British',
                 ETHGBEUL %in% 11       ~ 'Other',
                 TRUE ~ NA_character_)) %>%  
      select(-ETHGBEUL) -> x
    
  }
  
  ## format time in country to intervals
  ## do we want just year of entry instead?
  x <- 
    x %>% 
    mutate(timeinUK = 
             case_when(
               CAMEYR %in% c(-8,-9) ~ NA_character_,
               TRUE ~ as.character(cut(year - CAMEYR,
                                       breaks = c(0,1,2,5,100)))
             )) %>% 
    select(-CAMEYR)
  
  ## formatting of sex
  x %>%
    mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                        ifelse(SEX == 1, 'Male', 'Female'))) %>%
    select(-SEX) -> x
  
  ## standardise weight
  x %>%
    rename(Weight = PWT14) -> x
  x
}

#
format_LFS_2012to2016 <- function(x, year) {

  ## clean age with R style missing
  x %>%
    mutate(Age = replace(AGE, AGE %in% c(-8,-9), NA)) %>%
    select(-AGE) -> x
  
  ## country of residence
  x %>%
    mutate(Country = 
             case_when(
               COUNTRY %in% c(-9,-8) ~ NA_character_,
               COUNTRY %in% 1        ~ 'England',
               COUNTRY %in% 2        ~ 'Wales',
               COUNTRY %in% c(3,4)   ~ 'Scotland',
               COUNTRY %in% 5        ~ 'Northern Ireland',
               TRUE ~ NA_character_)) %>%  
    select(-COUNTRY) -> x
  
  ## country of birth (UK/not UK)
  UK_codes <- c(921, 922, 923, 924, 926)
  x <- 
    x %>%
    mutate(CoB = ifelse(CRY12 %in% c(-8,-9), NA,
                        ifelse(CRY12 %in% UK_codes,
                               yes = 'UK born',
                               no  = 'Non-UK born'))) %>%
    select(-CRY12)
  
  ## formatting of sex
  x %>%
    mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                        ifelse(SEX == 1,
                               yes = 'Male',
                               no  = 'Female'))) %>%
    select(-SEX) -> x
  
  ## format ethnicity
  x %>%
    mutate(ethgrp = 
             case_when(
               ETHGBEUL %in% c(-9,-8) ~ NA_character_,
               ETHGBEUL %in% c(1,2,3) ~ 'White',
               ETHGBEUL %in% 4        ~ 'Mixed',
               ETHGBEUL %in% 5        ~ 'Indian',
               ETHGBEUL %in% 6        ~ 'Pakistani',
               ETHGBEUL %in% 7        ~ 'Bangladeshi',
               ETHGBEUL %in% c(8,9)   ~ 'Asian',
               ETHGBEUL %in% 10       ~ 'Black/Black British',
               ETHGBEUL %in% 11       ~ 'Other',
               TRUE ~ NA_character_)) %>%  
    select(-ETHGBEUL) -> x
  
  ## format time in country to intervals
  ## do we want just year of entry instead?
  x <- 
    x %>% 
    mutate(timeinUK = 
             case_when(
               CAMEYR %in% c(-8,-9) ~ NA_character_,
               TRUE ~ as.character(cut(year - CAMEYR,
                                       breaks = c(0,1,2,5,100)))
             )) %>% 
    select(-CAMEYR)
  
  ## format country of birth
  ##TODO: do we need this or just use UK/non-UK?
  ## some duplication with ethnicity
  
  x <- rename_at(x, vars(contains("PWT")), ~ "Weight")
  x
}
