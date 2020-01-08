#
plot_LFS <- function(form_LFS_data) {
  
  # Simple plots of the data ------------------------------------------------
  ## 2000 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2000) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p
  
  p
  
  
  ## 2005 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2005) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p1
  
  p1
  
  
  ## 2010 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2010) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p2
  
  p2
  
  
  
  ## 2015 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2015) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p3
  
  interactive_plot(p3, interactive)
  
  ## 2000 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2000) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p4
  
  p4
  
  ## 2005 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2005) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p5
  
  p5
  
  ## 2010 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2010) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p6
  
  p6
  
  ## 2015 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2015) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme_set() +
    theme(axis.text.x = element_text(angle = 90)) -> p7
  
  p7
}