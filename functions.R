# INITIAL DATA INSPECTION:

# 2.1: function for NA stats:

count_NA <- function(df){
  
  require(tidyverse)
  require(cowplot)
  
  # counting NAs, and their percentages per column:
  df_NA_count <- map(df, ~sum(is.na(.))) %>% 
    simplify() %>% 
    tibble(col = names(.),
           NAs = .) %>% 
    mutate(NA_pct = round(NAs / nrow(df) * 100, 2))
  
  print(df_NA_count %>% 
          as.data.frame())
  
  # absolute numbers for NA:
  plot1 <- ggplot(df_NA_count, aes(x = col, y = NAs)) +
    geom_col(width = 0.8) +
    ggtitle('NA Missing Count') +
    labs(x = 'Column',
         y = 'Count of NAs Missing') +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0,NaN)) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, size = 6),
          axis.title.x = element_text(size = 9, face = 'italic'),
          axis.title.y = element_text(size = 9, face = 'italic'),
          plot.title = element_text(face = 'bold'))
  
  # relative numbers for NA:
  plot2 <- ggplot(df_NA_count, aes(x = col, y = NA_pct)) +
    geom_col(width = 0.8) +
    scale_y_continuous(limits = c(0,100),
                       expand = c(0,0)) +
    labs(x = 'Column',
         y = 'Percentage of NAs Missing') +
    ggtitle('NA Relative Numbers') +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, size = 6),
          axis.title.x = element_text(size = 9, face = 'italic'),
          axis.title.y = element_text(size = 9, face = 'italic'),
          plot.title = element_text(face = 'bold'))
  
  plot_grid(plot1, plot2, nrow = 2)
}





# 2.2: function for timespans:

df_timespan_Check <- function(df){
  
  require(tidyverse)
  
  # summarize the time span at the table level:
  table_count <- df %>% 
    summarise(distinct_dates = n_distinct(date),
              min_date = min(date),
              max_date = max(date),
              min_max_day_diff = max_date - min_date)
  
  print(table_count)
  
  # visualizes the number of date values at the state level:
  
  df %>% 
    group_by(state) %>% 
    summarise(distinct_dates = n_distinct(date)) %>% 
    ungroup() %>% 
    ggplot(aes(x = state, y = distinct_dates)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))

}




# 3: Function for state matching:

state_matcher <- function(states_base = states_list,
                          data, 
                          col_name){
  
  require(tidystringdist)
  
  # final column for states
  col_name <- rlang::ensym(col_name)
  
  # extracting unique states from the inputted data 
  states_data <- data %>% 
    distinct(state)
  
  # create a table of all possible combinations
  states_comb <- expand.grid(states_base = states_list %>% 
                               pull(state_base),
                             state = states_data %>% 
                               pull(state)
                             )
  
  # compute string distance:
  state_distance <-
  tidy_stringdist(df = states_comb,
                  v1 = states_base,
                  v2 = state, method = 'osa') %>% 
    
    # lowest 'osa' is preferable
    arrange(states_base, osa) %>% 
    group_by(states_base) %>% 
    mutate(rank = row_number()) %>% 
    ungroup() %>% 
    
    # top ranks will be filtered:
    filter(rank == 1) %>% 
    select(states_base, !!col_name :=state)
    
    return(state_distance)
  
}


# 4: Exploration of the final data set:
# 4.1: Function for confirmed cases and deaths plots per region (actual and relative)

plot_confirmed_cases_total <- function(region_group){
  
  # data to be used:
  plot_data <- df_main %>% 
    filter(`region-group` == region_group)
  
  # plot: confirmed cases absolute count:
  p11 <- plot_data %>% 
    ggplot(aes(x = date,
               y = confirmed_total,
               group = state,
               color = state)) +
    geom_line(linewidth = 0.8, show.legend = FALSE) +
    geom_point(show.legend = FALSE, size = 0.7) +
    scale_color_viridis_d(option = 'C') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    labs(
         y = 'Number of Confirmed Cases Total',
         title = paste0('Infected Cases : ', region_group)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = 'italic'),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 8, face = 'italic'),
          axis.title.y = element_text(face = 'italic'))
  
  # plot: confirmed cases relative numbers:
  p12 <- plot_data %>% 
    ggplot(aes(x = date,
               y = confirmed_total_pct,
               group = state,
               color = state)) +
    geom_line(linewidth = 0.8, show.legend = FALSE) +
    geom_point(show.legend = FALSE, size = 0.7) +
    scale_color_viridis_d(option = 'C') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    labs(x = 'Date',
         y = '% of Confirmed Cases Total') +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = 'italic'),
          axis.title.x = element_text(face = 'italic'),
          axis.text.y = element_text(size = 8, face = 'italic'),
          axis.title.y = element_text(face = 'italic'))
  
  # plot: confirmed deaths absolute count:
  p21 <- plot_data %>% 
    ggplot(aes(x = date,
               y = deaths_total,
               group = state,
               color = state)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.7) +
    scale_color_viridis_d(option = 'C') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    labs(
         y = 'Number of Deaths Total',
         title = paste0('Death Count : ', region_group)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = 'italic'),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 8, face = 'italic'),
          axis.title.y = element_text(face = 'italic'),
          legend.title.position = 'top')
  
  # plot: confirmed deaths relative numbers:
  p22 <- plot_data %>% 
    ggplot(aes(x = date,
               y = confirmed_deaths_total_pct,
               group = state,
               color = state)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 0.7) +
    scale_color_viridis_d(option = 'C') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    labs(x = 'Date',
         y = '% of Deaths Total') +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8, face = 'italic'),
          axis.title.x = element_text(face = 'italic'),
          axis.text.y = element_text(size = 8, face = 'italic'),
          axis.title.y = element_text(face = 'italic'),
          legend.title = element_blank())
  
  # final subplot:
  plot_grid(p11, p21, p12, p22, nrow = 2, ncol = 2)

}



# 4.2: Time series for daily cases and deaths (7 day average):

plot_confirmed_cases_deaths_7d_avg <- function(region_group){
  
  # data:
  plot_data <- df_main %>% 
    filter(`region-group` == region_group)
  
  # confirmed cases:
  p1 <- plot_data %>% 
    ggplot(aes(x = date, y = confirmed_daily_cases_7_day_average,
               group = state,
               colour = state)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1,
               show.legend = FALSE,
               shape = 16) +
    geom_segment(aes(x = as.Date('2021-01-13'),
                     xend = as.Date('2021-01-13'),
                     y = -Inf,
                     yend = Inf), 
                 color = 'black',
                 lineend = 'square') +
    scale_color_viridis_d(option = 'Magma') +
    labs(x = 'Date',
         y = 'Number of Cases (7 Day Average)',
         title = paste0('Confirmed Daily Cases / ', region_group)) +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    theme_light() +
    theme(axis.title = element_text(face = 'italic'),
          axis.title.x = element_blank())
  
  # confirmed deaths:
  p2 <- plot_data %>% 
    ggplot(aes(x = date, y = confirmed_deaths_7_day_average,
               group = state,
               colour = state)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1,
               show.legend = FALSE,
               shape = 16) +
    geom_segment(aes(x = as.Date('2021-01-13'),
                 xend = as.Date('2021-01-13'),
                 y = -Inf,
                 yend = Inf), 
                 color = 'black',
                 linend = 'square') +
    scale_color_viridis_d(option = 'Magma') +
    labs(x = 'Date',
         y = 'Number of Deaths (7 Day Average)',
         title = paste0('Confirmed Daily Deaths / ', region_group),
         caption = 'Vaccinations rollout: 2021-01-13') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    theme_light() +
    theme(axis.title = element_text(face = 'italic'),
          plot.caption = element_text(size = 8, face = 'italic',
                                      hjust = 0.5, vjust = 4))
    
  plot_grid(p1, p2, nrow = 2)
}



# 4.3: Checking the 7 day average for confirmed cases but 
# including the vaccine doses as well:

plot_confirmed_cases_deaths_7d_avg_vaccine_doses_total <- function(region_group){
  
  # data:
  plot_data <- df_main %>% 
    filter(`region-group` == region_group)
  
  # confirmed cases:
  p1 <- plot_data %>% 
    ggplot(aes(x = date, y = confirmed_daily_cases_7_day_average,
               group = state,
               colour = state)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1,
               show.legend = FALSE,
               shape = 16) +
    geom_segment(aes(x = as.Date('2021-01-13'),
                     xend = as.Date('2021-01-13'),
                     y = -Inf,
                     yend = Inf), 
                 color = 'black',
                 lineend = 'square') +
    scale_color_viridis_d(option = 'Magma') +
    labs(x = 'Date',
         y = 'Number of Cases (7 Day Average)',
         title = paste0('Confirmed Daily Cases / ', region_group)) +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    theme_light() +
    theme(axis.title = element_text(face = 'italic'),
          axis.title.x = element_blank())
  
  # total deaths:
  p2 <- plot_data %>% 
    ggplot(aes(x = date, y = confirmed_deaths_7_day_average,
               group = state,
               colour = state)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1,
               show.legend = FALSE,
               shape = 16) +
    geom_segment(aes(x = as.Date('2021-01-13'),
                     xend = as.Date('2021-01-13'),
                     y = -Inf,
                     yend = Inf), 
                 color = 'black',
                 linend = 'square') +
    scale_color_viridis_d(option = 'Magma') +
    labs(x = 'Date',
         y = 'Number of Deaths (7 Day Average)',
         title = paste0('Confirmed Daily Deaths / ', region_group),
         caption = 'Vaccinations rollout: 2021-01-13') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    theme_light() +
    theme(axis.title = element_text(face = 'italic'),
          plot.caption = element_text(size = 8, face = 'italic',
                                      hjust = 0.5, vjust = 4))
  
  
  # Total vaccine doses:
  p3 <- plot_data %>% 
    ggplot(aes(x = date, y = vaccine_doses_total,
               group = state,
               colour = state)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1,
               show.legend = FALSE,
               shape = 16) +
    geom_segment(aes(x = as.Date('2021-01-13'),
                     xend = as.Date('2021-01-13'),
                     y = -Inf,
                     yend = Inf), 
                 color = 'black',
                 linend = 'square') +
    scale_color_viridis_d(option = 'Magma') +
    labs(x = 'Date',
         y = 'Number of Vaccine Doses',
         title = paste0('Total Vaccine Doses / ', region_group),
         caption = 'Vaccinations rollout: 2021-01-13') +
    scale_x_date(date_breaks = '2 month',
                 date_labels = '%b %Y') +
    theme_light() +
    theme(axis.title = element_text(face = 'italic'),
          plot.caption = element_text(size = 8, face = 'italic',
                                      hjust = 0.5, vjust = 4))
  
  plot_grid(p1, p2, p3, nrow = 3)
}



# 4.4: function for key metrics visualization per state:

plot_state_indicators <- function(st){
  
  # data:
  df_main_state <- df_main %>% 
    filter(state == st)
    
    # totals:
    p1 <- df_main_state %>% 
      select(date,
             confirmed_total,
             deaths_total,
             vaccine_doses_total) %>%
      rename(`Total Vaccine Doses`= 'vaccine_doses_total',
             `Total Confirmed Cases`= 'confirmed_total',
             `Total Deaths` = 'deaths_total') %>% 
      pivot_longer(cols = c('Total Confirmed Cases',
                            'Total Deaths',
                            'Total Vaccine Doses'),
                   names_to = 'key_totals',
                   values_to = 'value') %>% 
      mutate(key_totals = factor(key_totals,
                                 levels = c('Total Vaccine Doses',
                                            'Total Confirmed Cases',
                                            'Total Deaths'))) %>%
      ggplot(aes(x = date,
                 y = value,
                 fill = key_totals)) +
      geom_area(color = 'black', alpha = 0.6) +
      labs(x = 'Date',
           y = 'Value',
           title = paste0('State: ', st, ' - COVID19 Key Indicators')) +
      scale_fill_discrete(type = c('violetred', 'firebrick', 'black'),
                          name = 'Key Totals') +
      scale_x_date(date_breaks = '2 month',
                   date_labels = '%b %Y') +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,NaN)) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.title = element_text(face = 'italic'))
    
    
    # 7 day averages:
    p2 <- df_main_state %>% 
      select(date,
             confirmed_daily_cases_7_day_average,
             confirmed_deaths_7_day_average,
             vaccine_doses_7d_average) %>%
      rename(`Vaccines 7 day Avg.`= 'vaccine_doses_7d_average',
             `Confirmed Cases 7 day Avg.`= 'confirmed_daily_cases_7_day_average',
             `Confirmed Deaths 7 day Avg.` = 'confirmed_deaths_7_day_average') %>% 
      pivot_longer(cols = c('Confirmed Cases 7 day Avg.',
                            'Confirmed Deaths 7 day Avg.',
                            'Vaccines 7 day Avg.'),
                   names_to = 'key_averages',
                   values_to = 'value') %>% 
      mutate(value = na_if(value,0)) %>% 
      mutate(key_averages = factor(key_averages,
                                 levels = c('Vaccines 7 day Avg.',
                                            'Confirmed Cases 7 day Avg.',
                                            'Confirmed Deaths 7 day Avg.'))) %>%
      ggplot(aes(x = date,
                 y = value,
                 group = key_averages,
                 color = key_averages)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.2,
                 show.legend = FALSE) +
      labs(x = 'Date',
           y = 'Value',
           title = paste0('State: ', st, ' - COVID19 Key Indicator Averages')) +
      scale_color_manual(values = c('violetred', 'firebrick', 'black'),
                          name = '7 Day Averages') +
      scale_x_date(date_breaks = '2 month',
                   date_labels = '%b %Y') +
      scale_y_log10() +
      theme_bw() +
      theme(axis.title = element_text(face = 'italic'),
            axis.title.x = element_blank())
    
    # Government Response metrics:
    p3 <- df_main_state %>% 
      select(date,
             government_response_index_for_display,
             containment_health_index_for_display,
             economic_support_index_for_display,
             stringency_index_for_display) %>%
      rename(`Government Response`= 'government_response_index_for_display',
             `Containment Health`= 'containment_health_index_for_display',
             `Economic Support` = 'economic_support_index_for_display',
             `Stringency` = 'stringency_index_for_display') %>% 
      pivot_longer(cols = c('Government Response',
                            'Containment Health',
                            'Economic Support',
                            'Stringency'),
                   names_to = 'key_indices',
                   values_to = 'value') %>% 
      mutate(value = na_if(value,0)) %>% 
      mutate(key_indices = factor(key_indices,
                                   levels = c('Government Response',
                                              'Containment Health',
                                              'Economic Support',
                                              'Stringency'))) %>%
      ggplot(aes(x = date,
                 y = value,
                 fill = key_indices)) +
      geom_area(color = 'black', alpha = 0.6) +
      labs(x = 'Date',
           y = 'Value',
           title = paste0('State: ', st, ' - COVID19 Government Reaction')) +
      scale_fill_viridis_d(option = 'cividis',
                         name = 'Reaction Indices',
                         direction = -1) +
      scale_x_date(date_breaks = '2 month',
                   date_labels = '%b %Y',
                   expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0),
                         limits = c(0,300)) +
      theme_bw() +
      theme(axis.title = element_text(face = 'italic'))
    
      plot_grid(p1, p2, p3, nrow = 3)
}

