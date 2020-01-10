#
plot_combined_ONS_LFS <- function(demo_2000_2016_strat_est){
  
  ## Plots of Non-UK born over time
  demo_2000_2016_strat_est %>%
    filter(Year %% 5 == 0, CoB %in% 'Non-UK born') %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~Year) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "Non-UK born population by age, every 5 years") -> p
  
  p
  
  ## Plots of UK born over time
  demo_2000_2016_strat_est %>%
    filter(Year %% 5 == 0, CoB %in% 'UK born') %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~Year) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "UK born population by age, every 5 years") -> p1
  
  p1
  
  
  ## Compare Population strat by year - 2000
  demo_2000_2016_strat_est %>%
    filter(Year %in% 2000, !is.na(CoB)) %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~CoB) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2000") -> p2
  
  p2
  
  
  ## Compare Population strat by year - 2005
  demo_2000_2016_strat_est %>%
    filter(Year %in% 2005, !is.na(CoB)) %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~CoB) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2005") -> p3
  
  p3
  
  
  ## Compare Population strat by year - 2010
  demo_2000_2016_strat_est %>%
    filter(Year %in% 2010, !is.na(CoB)) %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~CoB) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2010") -> p4
  
  p4
  
  
  ## Compare Population strat by year - 2015
  demo_2000_2016_strat_est %>%
    filter(Year %in% 2015, !is.na(CoB)) %>%
    ggplot(aes(x = Age, y = Population)) +
    geom_density(stat = "identity", alpha = 0.4) +
    facet_wrap(~CoB) +
    theme_set +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(caption = "Comparision of ONS, LFS, UK born,
          and non-UK born population estimates for 2015") -> p5
  
  p5
  
  
  # Look at total population over time --------------------------------------
  demo_2000_2016_strat_est %>%
    # filter(Year %% 5 == 0) %>%
    group_by(CoB, Year) %>%
    summarise(Population = sum(Population)) %>%
    ggplot(aes(x = Year, y = Population, fill = CoB, colour = CoB)) +
    geom_point() +
    geom_line() +
    theme_set +
    labs(caption = "Population estimates over time for the ONS and LFS") -> p6
  
  p6
}