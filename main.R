
#-------------------------------------------------------------------------------
library(tidyverse) 
library(lubridate) 
library(rio) # for import()
library(cowplot) 
library(janitor)
library(ggplot2)
library(zoo) 
library(viridis) # viridis color scale options
#-------------------------------------------------------------------------------



# Importing all relevant data:
# All data used is stored in this projects directory
# and will be accessed through that end

path_origin <- './data'











#------------------------------------- main data:-------------------------------
#-------------------------------- 1.1: COVID-19 data:---------------------------

data_dir_name_COVID19 <- list.files(path = path_origin) %>% 
  str_subset("^csse_")

# first tibble for COVID19 data:
df_COVID19 <- 
  
  # The first step is to create a directory for the data:
  tibble(directory = paste(path_origin,"/",
                           data_dir_name_COVID19,
                           sep = ''),
                     file = list.files(path = directory)) %>% 
  
  # A new path column is added through concatenating directory and file
  mutate(path = str_c(directory, file, sep = '/')) %>% 
  
  # Each path has its csv read for using the below function, with all columns
  # parsed as character, further parsing will be done later
  mutate(data = map(.x = path,
                    .f = function(path_){
                      read_csv(path_, col_types = cols(.default = "c"))
                    }
                    )) %>% 
  
  # the date for each file can be subset from the file column
  # the date column is simultaneously
  mutate(date = str_subset(string = file,
                           pattern = '\\.csv'),
         date = mdy(date)) %>% 
  
  # directory, path and file are no longer needed
  select(date, data) %>% 
  
  # un-nesting CSV's into the upper layer:
  unnest(cols = data) %>% 
  clean_names()

#-------------------------- 1.1.1: COVID-19 data cleaning:----------------------
df_COVID19 <- df_COVID19 %>% 
  
  mutate(last_update = ymd_hms(last_update)) %>% 
  
  # iso3 needs to remain a character column while the remaining from 
  # the 5th column onward can be set to be numeric
  mutate_at(.tbl = .,
            .vars = setdiff(colnames(.)[5:ncol(.)], 'iso3'),
            .funs = as.numeric) %>% 
  rename(state = province_state)













#-------------------------------------------------------------------------------
#--------------------------------- 1.2: GDP_Data:-------------------------------

data_dir_name_GDP <- list.files(path = path_origin) %>% 
  str_subset('GDP')

# rio::import() is used here for excel import, specifically the 'clean data' 
# sheet in the workbook

df_GDP <- import(file = paste0(path_origin, '/', data_dir_name_GDP),
                 sheet = 'clean data') %>% 
  clean_names()


#------------------------------ 1.2.1: GDP_Data cleaning:-----------------------

df_GDP <- df_GDP %>% 
  
  # selecting required columns for the analysis: 
  select(state = state_or_territory,
         gdp_nominal = nominal_gdp_2020,
         gdp_per_capita = gdp_per_capita_2020) %>% 
  
  # getting rid of special characters from the data ()
  mutate_all(., .funs = str_remove_all, ",") %>% 
  mutate_all(., .funs = str_remove_all, "\\$") %>% 
  
  # parsing the 2nd and 3rd columns as numeric
  mutate_at(., .vars = colnames(.)[2:3], .funs = as.numeric)










#-------------------------------------------------------------------------------
#------------------------------ 1.3: Population data

data_dir_name_population <- list.files(path = path_origin) %>% 
  str_subset('Population')

# rio::import() for extracting the excel sheet data:
df_population <- import(file = paste0(path_origin,'/', data_dir_name_population),
                 sheet = 'data') %>% 
  clean_names()

# further cleaning:
df_population <- df_population %>% 
  select(state = name,
         population = pop_2019)










#-------------------------------------------------------------------------------
#--------------------------- 1.4: Covid-19 Response data:-----------------------

data_dir_name_response <- list.files(path = path_origin) %>% 
  str_subset('US_latest')

# reading the OxCGRT_US response data:
df_response <- read_csv(file = paste0(path_origin,'/',data_dir_name_response),
                        col_types = cols(.default = 'c')) %>% 
  clean_names()

# further cleaning:
df_response <- df_response %>% 
  mutate(date = ymd(date)) %>% 
  
  # selecting the states, date, and index data columns
  select(state = region_name,
         date,
         contains('index')) %>% 
  
  # the index data starts from column no. 3:
  mutate_at(.tbl = .,
            .vars = colnames(.)[3:ncol(.)],
            .funs = as.numeric)









#-------------------------------------------------------------------------------
#----------------------------- 1.5: Vaccination data:---------------------------

data_dir_name_vaccination <- list.files(path = path_origin) %>% 
  str_subset('-vaccine')

df_vaccinations <- read_csv(file = paste0(path_origin,
                                          '/',
                                          data_dir_name_vaccination),
                            col_types = cols(.default = 'c')) %>% 
  clean_names()

df_vaccinations <- df_vaccinations %>% 
  mutate(date = ymd(day),
         daily_vaccinations = as.numeric(daily_vaccinations)) %>%
  select(state = entity,
         date = day,
         vaccinations = daily_vaccinations) %>% 
  mutate(date = as.Date(date))

  









#-------------------------------------------------------------------------------
#---------------------------- 2: DATA INSPECTION:-------------------------------

# custom functions:
source('./functions.R')

#---------------------------- 2.1: Missing Data:--------------------------------

count_NA(df_COVID19)
# 25% of active is missing, hence the column is unreliable
# deaths and confirmed have 0 missing entries and will
# definitely be used

count_NA(df_GDP)
# 0 missing values

count_NA(df_population)
# 0 missing values

count_NA(df_response)
# the absolute count is mostly in the 100-500 range
# but their relative count is negligible

count_NA(df_vaccinations)
# 0 missing values









#-------------------------------------------------------------------------------
#----------------------------- 2.2: Time span Validation:-----------------------

df_timespan_Check(df_COVID19)
# the only column out of the norm is Puerto Rico
# which will not be considered as it is a common wealth region

df_timespan_Check(df_response)
# no anomalies

df_timespan_Check(df_vaccinations)
# United states is an entry in the state leading to an anomaly











#-------------------------------------------------------------------------------
#----------------------------- 2.3: Main data Inspection:-----------------------

# confirmed, deaths, confirmed and active are important
# Consistency check
# - On a National level
# - on a state level

df_COVID19 %>% 
  
  # subset of relevant data
  select(state, date, confirmed:active) %>%
  
  # test filter condition to check consistency
  filter(state == 'Alabama') %>% 
  pivot_longer(cols = c('confirmed', 'deaths', 'recovered', 'active'),
               names_to = 'variable',
               values_to = 'value') %>% 
  
  # multiples indicating each column
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_point() +
  facet_grid(variable~., scales = 'free')

# NOTE: The above code indicates that active and recovered are unreliable.
#       The key metrics by which the analysis is done will be the total cases,
#       deaths and vaccinations.


df_COVID19 %>% 
  select(state, date, confirmed:active) %>%
  pivot_longer(cols = c('confirmed', 'deaths', 'recovered', 'active'),
               names_to = 'variable',
               values_to = 'value') %>%
  
  # data grouped by date and variable
  # for daily aggregates
  group_by(date, variable) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_point() +
  facet_grid(variable~., scales = 'free')

# NOTE: The above inspections indicate that we currently only have running totals
# for the key metrics, indicated by the upwards trend of the plots.
# The daily counts have to be calculated for more a granular look at the 
# situation. confirmed and deaths have been reportedly consistently while
# active and recovered are quite inconsistent and hence unreliable.










#-------------------------------------------------------------------------------
#-------------------- 3: WRANGLING FOR MAIN TABLE CREATION:---------------------

# A single master table will be made for the final analyses
# the table will be made with the following steps:
# - the data will be grouped by state and date
#   - that is, there will be one entry per date per state
# - all previously mentioned relevant data will be appended to this table.
# - Steps to be followed:
#   - create a list of all of America's states
#   - a function will be created to:
#     - match data by state names
#     - string similarity matching will be done
#     - since the data frames created for the different data might have different
#     - state names, spelling, etc...


# USA states list:
states_list <- tibble(state_base = state.name)

# do the state names match with the function state_matcher():
states_list_COVID19 <- state_matcher(data = df_COVID19, col_name = 'state_COVID19')
states_list_GDP <- state_matcher(data = df_GDP, col_name = 'state_GDP')
states_list_population <- state_matcher(data = df_population, col_name = 'state_population')
states_list_response <- state_matcher(data = df_response, col_name = 'state_response')
states_list_vaccinations <- state_matcher(data = df_vaccinations, col_name = 'state_vaccinations')

# creating a universal states list:
states_list_u <- states_list_COVID19 %>% 
  inner_join(x = .,
             y = states_list_GDP,
             by = 'states_base') %>%  
  inner_join(x = .,
             y = states_list_population,
             by = 'states_base') %>%  
  inner_join(x = .,
             y = states_list_response,
             by = 'states_base') %>%  
  inner_join(x = .,
             y = states_list_vaccinations,
             by = 'states_base') %>% 
  arrange(states_base) %>% 
  mutate(state_id = row_number()) %>% 
  select(state_id, everything())

# the above universal states list has an issue where in states_list_vaccinations
# new jersey appears in place of New York
# this needs correctional imputation

states_list_vaccinations %>% 
  filter(states_base %in% c('New Jersey', 'New York'))

states_list_u[states_list_u$states_base == 'New York', 'state_vaccinations'] <- 'New York State'


# adding state regions:
# state regions are added so that each has a subset of states to itself
# this will make visualization easier down the line!
states_region <- tibble(states_base = state.name,
                        region = state.region)

df_states <- states_list_u %>% 
  left_join(x = .,
            y = states_region,
            by = 'states_base')

# adding the relevant dates:
df_dates <- tibble(date = seq.Date(from = df_COVID19 %>% pull(date) %>% min(),
                                   to = df_COVID19 %>% pull(date) %>% max(),
                                   by = 'day'))

# creating a main table:
df_main <- df_states %>% 
  full_join(x = .,
            y = df_dates,
            by = character())

# checking the main table for the expected data:
df_main %>% 
  count(states_base) %>% 
  as.data.frame()

# the relevant data must now be brought in from the the different sources:
df_main <- df_main %>% 
  left_join(x = .,
            y = df_COVID19 %>% 
              select(state, date, confirmed, deaths),
            by = c('state_COVID19' = 'state',
                   'date' = 'date')) %>% 
  left_join(x = .,
            y = df_GDP,
            by = c('state_GDP' = 'state')) %>% 
  left_join(x = .,
            y = df_population,
            by = c('state_population' = 'state')) %>% 
  left_join(x = .,
            y = df_vaccinations %>% 
              select(state, date, vaccinations),
            by = c('state_vaccinations' = 'state',
                   'date' = 'date')) %>% 
  left_join(x = .,
            y = df_response,
            by = c('state_response' = 'state',
                   'date' = 'date')) 
  
# further cleaning of df_main
# redundant field removal:
df_main$state_COVID19 <- NULL
df_main$state_GDP <- NULL
df_main$state_population <- NULL
df_main$state_vaccinations <- NULL
df_main$state_response <- NULL

# Rearrangement and renaming:
df_main <- df_main %>% 
  select(state_id,
         state = states_base,
         region,
         date,
         `confirmed total` = confirmed,
         `deaths total` = deaths,
         `daily vaccine doses administered` = vaccinations,
         everything()) %>%
  arrange(state, date)


# Additional wrangling:
# - population in millions column
# - daily counts for different metrics (cases, deaths)
# - flagging the first date when the vaccinations started
# - days where vaccination is missing after its started
# - ... and additional cleaning.

df_main %>% 
  filter(is.na(`confirmed total`)) %>% 
  nrow()
# 0 rows missing data

df_main %>% 
  filter(is.na(`deaths total`)) %>% 
  nrow()  
# 0 rows missing data

df_state_vaccination_date_min %>% 
  df_main %>% 
  filter(!is.na(`daily vaccine doses administered`)) %>% 
  group_by(state) %>% 
  summarise(start_date = min(date)) %>% 
  ungroup() %>% 
  select(start_date) %>% 
  unique()
# The start date appears to be the 13th of January 2021

# list of vaccination start dates per state
df_state_vaccination_date_min <- 
  df_main %>% 
  filter(!is.na(`daily vaccine doses administered`)) %>% 
  group_by(state) %>% 
  summarise(start_date = min(date)) %>% 
  ungroup()



df_main <- df_main %>% 
  
  # population -> population_in_millions
  mutate(population_in_millions = round(population/ 10 * 6, 2)) %>% 
  
  # replacing the NAs in 'daily vaccine doses administered' and creating a new column
  mutate(daily_vaccine_doses = replace_na(`daily vaccine doses administered`, 0)) %>% 
  
  # daily count for key metrics
  # the rolling data is what has been provided
  group_by(state) %>% 
  mutate(confirmed_daily_cases = `confirmed total` - lag(`confirmed total`, 1),
         confirmed_daily_deaths = `deaths total` - lag(`deaths total`, 1)) %>% 
  
  # total count for `daily vaccine doses administered`:
  mutate(vaccine_doses_total = cumsum(daily_vaccine_doses)) %>% 
  ungroup() %>% 
  
  # final rearrangement:
  select(state_id:date,
         `confirmed total`, confirmed_daily_cases,
         `deaths total`, confirmed_daily_deaths,
         vaccine_doses_total, daily_vaccine_doses,
         everything()) %>% 
  
  # final renaming:
  rename(confirmed_total = `confirmed total`,
         deaths_total = `deaths total`)

# removing the redundant `daily vaccine doses administered' field`
df_main$`daily vaccine doses administered` <- NULL



# a quick glance through df_main indicates some negative values:
df_main %>% filter(confirmed_daily_cases < 0)
df_main %>% select(confirmed_daily_deaths) %>% filter(confirmed_daily_deaths < 0)
df_main %>% select(date, daily_vaccine_doses) %>% filter(daily_vaccine_doses < 0)



# cleaning up the -ve values:
df_main <- df_main %>% 
  
  mutate(confirmed_daily_cases = case_when(confirmed_daily_cases >= 0 ~ confirmed_daily_cases,
                                           TRUE ~ 0),
         confirmed_daily_deaths = case_when(confirmed_daily_deaths >= 0 ~ confirmed_daily_deaths,
                                           TRUE ~ 0),
         daily_vaccine_doses = case_when(daily_vaccine_doses >= 0 ~ daily_vaccine_doses,
                                           TRUE ~ 0))
  



#-------------------------------------------------------------------------------
# -------------------------- 4: Exploration of the final data set:--------------


# state count by region:
df_main %>% 
  group_by(region) %>% 
  summarise(states = n_distinct(state))

max_date <- df_main %>% 
  pull(date) %>% 
  max(.) # '2021-06-13'

# creating a new states column for easier to access and appropriately named states
df_main <- df_main %>% 
  mutate(state_ = str_to_lower(state))

# to create a map:
df_main %>% 
  filter(date == max_date) %>% 
  
  # for the required data for geospatial mapping:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # this gives us a view of the different states mapped to each region
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = region),
               color = 'black',
               linewidth = 0.08) +
  coord_quickmap() +
  theme_void() +
  scale_fill_viridis_d(option = 'D', alpha = 0.7)


# Q1: What is happening with the confirmed total infections on the state level over time?
# Q2: What do the confirmed total deaths look like at the state level overtime?

df_main <- df_main %>% 
  mutate(confirmed_total_pct = confirmed_total / population,
         confirmed_deaths_total_pct = deaths_total / population)

# splitting the states by region:
df_region_group <- df_main %>%
  
  # subset for relevant data
  select(region, state) %>% 
  group_by(region) %>%
  
  # derive the count of states per region
  mutate(states = n_distinct(state)) %>% 
  
  # grab the unique row combinations
  distinct() %>% 
  ungroup() %>% 
  arrange(region, state) %>% 
  
  # adding a row index:
  group_by(region) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  
  # adding a group id:
  mutate(group = case_when(id <= round(states / 2, 0) ~ 1, TRUE ~ 2)) %>% 
  mutate(`region-group` = paste0(region, "-group", group)) %>% 
  
  # final selection for groups:
  select(region, state, `region-group`)

# bringing in the new groups into the main table:
df_main <- df_main %>% 
  left_join(x = .,
            y = df_region_group %>% select(-region),
            by = 'state')


# A1 and A2: 

# for easy mapping of the different region groups for when the data is visualized:
region_group_pairs <- df_region_group %>% 
  distinct(`region-group`) %>% 
  pull(`region-group`)
  

plot_confirmed_cases_total('West-group2') # testing the plotting function
                                          # for the cases in different region
                                          # groups

# the different plots for each region-group:
# these plots will be exported to a new folder created in the project
# directory called 'exploration_visuals'
for(pair in region_group_pairs){
  
ggsave(plot = plot_confirmed_cases_total(pair),
       file = paste0('./exploration_visuals/01_confirmed_cases_deaths_',pair,'.png'),
       width = 30, height = 20, units = 'cm')
}
  

# Q3: Which state paid the highest price in terms of overall cases and deaths?
# A3.1:
rel_plot_cases_deaths1 <- df_main %>% 
  
  # subset for the final day for the final numbers:
  filter(date == max_date) %>% 
  select(region, state, confirmed_total_pct, confirmed_deaths_total_pct) %>% 
  rename(`Confirmed Total Cases %` = "confirmed_total_pct",
         `Deaths Total %` = "confirmed_deaths_total_pct") %>% 
  
  # pivoting the table to a longer format for easier faceting:
  pivot_longer(cols = c(`Confirmed Total Cases %`, `Deaths Total %`),
               names_to = "count",
               values_to = "value") %>% 
  
  # sorting the states by highest damages incurred:
  group_by(state) %>% 
  mutate(total_pct = sum(value)) %>% 
  ungroup() %>% 
  arrange(total_pct, region) %>%
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  
  # bar plot for the relative loss of life and infections combined:
  ggplot(., aes(x = value, y = state, fill = region)) +
  geom_col(color = 'black', width = 0.8, linewidth = 0.5) + 
  scale_fill_viridis_d(name = 'Region',option = 'D') +
  facet_wrap(count~., scales = 'free') +
  labs(x = 'Total %', y = 'State',
       caption = paste0('As on ', max_date)) + 
  ggtitle('Overall Losses') +
  theme(axis.text.y = element_text(size = 10, face = 'italic'),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 8, face = 'italic',
                                    hjust = 0.5, vjust = 4),
        axis.title = element_text(size = 12))

ggsave(plot = rel_plot_cases_deaths1,
       file = paste0('./exploration_visuals/02_relative_count_confirmed_cases_deaths_final_date.png'),
       width = 30, height = 20, units = 'cm')

# A3.2:  
# Creating a map for `Deaths Total %` vs. Population :
p1 <- df_main %>%
  filter(date == max_date) %>% 
  rename(`Confirmed Total Cases %` = "confirmed_total_pct",
         `Deaths Total %` = "confirmed_deaths_total_pct") %>% 
  
  # map data retrieval:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # map plotting:
  # state_centers <- read.csv('.data/state_centers_data.csv')
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = `Deaths Total %`, group = group),
               color = 'black',
               linewidth = 0.08) +
  theme_void() +
  ggtitle('% Deaths VS State Population') + 
  coord_quickmap() +
  geom_text(data = state_centers,
            aes(x = avg_long,
                y = avg_lat, 
                label = state_initials), 
            size = 3,
            fontface = 'bold',
            color = 'white') +
  scale_fill_viridis_c(direction = -1, option = 'magma') +
  theme(plot.title = element_text(hjust = 0.6))
  
# Creating a map for `Confirmed Total Cases %` vs. Population :
p2 <- df_main %>%
  filter(date == max_date) %>% 
  rename(`Confirmed Total Cases %` = "confirmed_total_pct",
         `Deaths Total %` = "confirmed_deaths_total_pct") %>% 
  
  # map data retrieval:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # map plotting:
  # state_centers <- read.csv('.data/state_centers_data.csv')
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(fill = `Confirmed Total Cases %`, group = group),
               color = 'black',
               linewidth = 0.08) +
  theme_void() +
  ggtitle('% Confirmed Cases VS State Population') + 
  coord_quickmap() +
  geom_text(data = state_centers,
            aes(x = avg_long,
                y = avg_lat, 
                label = state_initials), 
            size = 3,
            fontface = 'bold',
            color = 'black') +
  scale_fill_gradient2(low = 'springgreen4',
                        mid = 'yellow',
                        high = 'firebrick',
                        midpoint = 0.0875,
                       name = 'Confirmed Cases %') +
  theme(plot.title = element_text(hjust = 0.6))

# Facet for the above two map plots for the relative deaths and cases:
rel_plot_cases_deaths2 <- cowplot::plot_grid(p1, p2, nrow = 2)
ggsave(plot = rel_plot_cases_deaths2,
       file = paste0('./exploration_visuals/02_relative_count_confirmed_cases_deaths_final_date_map.jpeg'),
       width = 35, height = 25, units = 'cm')






# Daily Dynamics:
# Q4: What do the daily confirmed cases look like?
# Q5: What do the daily Deaths look like?
# Q6: What do their rolling 7 day Averages look like?

# A4, A5, A6:
df_main <- df_main %>%
  
  # setting up the rolling 7 day average for the confirmed cases and deaths:
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate('confirmed_daily_cases_7_day_average' = rollapply(confirmed_daily_cases,
                                                           FUN = mean,
                                                           width = 7,
                                                           align = 'right',
                                                           fill = NA),
         'confirmed_deaths_7_day_average' = rollapply(confirmed_daily_deaths,
                                                      FUN = mean,
                                                      width = 7,
                                                      align = 'right',
                                                      fill = NA)) %>% 
  ungroup()
  
region_group_pairs
plot_confirmed_cases_deaths_7d_avg("Northeast-group1")

for (pair in region_group_pairs) {
  ggsave(plot = plot_confirmed_cases_deaths_7d_avg(pair),
         file = paste0('./exploration_visuals/03_confirmed_cases_deaths_7d_avg_',pair,'.png'),
         width = 30, height = 20, units = 'cm', dpi = 600)
}






# Q7: What is the correlation between GDP, population and total percentages of cases and deaths?
# A7:
plot_confirmed_cases_deaths_vs_gdp_pop <- df_main %>% 
  filter(date == max_date) %>% 
  ggplot(aes(x = confirmed_total_pct,
             y = confirmed_deaths_total_pct,
             size = population,
             color = gdp_per_capita)) +
  geom_point(alpha = 0.5) +
  facet_wrap(.~region)  +
  theme_light() +
  scale_color_gradientn(colors = c('firebrick', 'yellow', 'springgreen4'),
                        name = 'GDP per Capita') +
  scale_size_area(max_size = 30,
                  name = 'Population') +
  labs(x = 'Total Confirmed Cases %',
       y = 'Total Deaths %',
       title = 'Total Confirmed Cases & Deaths % VS. Population & GDP',
       caption = paste0('As on: ', max_date)) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.title = element_text(face = 'italic'),
        plot.caption = element_text(size = 8, face = 'italic',
                                    hjust = 0.5, vjust = 4))

ggsave(plot = plot_confirmed_cases_deaths_vs_gdp_pop,
       file = paste0('./exploration_visuals/04_confirmed_cases_deaths_vs_gdp_population','.png'),
       width = 30, height = 25, units = 'cm')


plot_confirmed_cases_deaths_vs_gdp <- df_main %>% 
  filter(date == max_date) %>% 
  ggplot(aes(x = confirmed_total_pct,
             y = confirmed_deaths_total_pct,
             size = gdp_per_capita,
             color = gdp_per_capita)) +
  geom_point(alpha = 0.5) +
  facet_wrap(.~region)  +
  theme_light() +
  scale_color_viridis_c(option = 'magma',
                        direction = -1,
                        name = 'GDP per Capita',
                        limits = range(df_main$gdp_per_capita)) +
  scale_size(range = c(1,12),
                  name = 'GDP per Capita',
                  limits = range(df_main$gdp_per_capita)) +
  guides(size = guide_legend(),
         color = guide_legend()) +
  labs(x = 'Total Confirmed Cases %',
       y = 'Total Deaths %',
       title = 'Total Confirmed Cases & Deaths % VS. GDP',
       caption = paste0('As on: ', max_date)) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.title = element_text(face = 'italic'),
        plot.caption = element_text(size = 8, face = 'italic',
                                    hjust = 0.5, vjust = 4))

ggsave(plot = plot_confirmed_cases_deaths_vs_gdp,
       file = paste0('./exploration_visuals/04_confirmed_cases_deaths_vs_gdp','.png'),
       width = 30, height = 25, units = 'cm')



# Q8: Do vaccinations help?
# Including the total vaccinations count to our previously visualized 7 day average
# might paint us a picture:
# A8:
region_group_pairs
plot_confirmed_cases_deaths_7d_avg_vaccine_doses_total("North Central-group2")

for (pair in region_group_pairs) {
  ggsave(plot = plot_confirmed_cases_deaths_7d_avg_vaccine_doses_total(pair),
         file = paste0('./exploration_visuals/05_confirmed_cases_deaths_7d_avg_vaccine_doses_',pair,'.png'),
         width = 30, height = 20, units = 'cm', dpi = 600)
}



# 9: Geographical representation of key values over time through maps:
# New columns will be added to df_main for creating snapshots of the country
# through the months indicating the fluctuations in deaths and cases:

df_main <- df_main %>% 
  arrange(state, date) %>% 
  
  # date_id per state:
  group_by(state) %>% 
  mutate(date_id = row_number()) %>% 
  ungroup() %>% 
  
  # adding a date snapshot flag:
  mutate(date_snapshot_flag = case_when(date_id == 1 ~ TRUE,
                                        date == max_date ~ TRUE,
                                        date_id %% 30 == 0 ~ TRUE,
                                        TRUE ~ FALSE))

# Creating the facet wrap for the multiple monthly snapshots:
# 9.1: confirmed cases over time:
plot_confirmed_cases_over_time <- df_main %>%
  filter(date_snapshot_flag) %>% 
  select(region, state_, date, confirmed_total) %>% 
  
  # map data retrieval:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # map plotting:
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = confirmed_total),
               color = 'black',
               linewidth = 0.08) +
  facet_wrap(.~date) +
  theme_bw() +
  ggtitle('Total Cases Over Time') +
  scale_fill_gradient2(low = 'yellowgreen',
                       mid = 'orange',
                       high = 'firebrick',
                       midpoint = 2000000,
                       name = 'Total Cases') +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(plot = plot_confirmed_cases_over_time,
       file = paste0('./exploration_visuals/06_confirmed_cases_over_time_map','.png'),
       width = 30, height = 20, units = 'cm')

# 9.2: vaccine doses over time:
plot_vaccine_doses_over_time <- df_main %>%
  filter(date_snapshot_flag) %>% 
  select(region, state_, date, vaccine_doses_total) %>% 
  
  # map data retrieval:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # map plotting:
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = vaccine_doses_total),
               color = 'black',
               linewidth = 0.08) +
  facet_wrap(.~date) +
  theme_bw() +
  ggtitle('Vaccine Doses Over Time') +
  scale_fill_gradient(low = 'white',
                       high = 'violetred3',
                       name = 'Total Cases') +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(plot = plot_vaccine_doses_over_time,
       file = paste0('./exploration_visuals/06_vaccine_doses_over_time_map','.png'),
       width = 30, height = 20, units = 'cm')


# 9.3: What did the response per state look like?
plot_containment_measures_over_time <- df_main %>%
  filter(date_snapshot_flag) %>% 
  select(region, state_, date, stringency_index_for_display) %>% 
  
  # map data retrieval:
  left_join(x = .,
            y = map_data('state'),
            by = c('state_' = 'region')) %>% 
  
  # map plotting:
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = stringency_index_for_display),
               color = 'black',
               linewidth = 0.08) +
  facet_wrap(.~date) +
  theme_bw() +
  ggtitle('State Response Rate Over Time') +
  scale_fill_viridis_c(option = 'magma',
                      name = 'Total Cases',
                      direction = -1) +
  theme(plot.title = element_text(hjust = 0.6),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(plot = plot_containment_measures_over_time,
       file = paste0('./exploration_visuals/06_containment_measures_over_time_map','.png'),
       width = 30, height = 20, units = 'cm')

# Q10: How are select states doing?
# A10:

# Adding a 7d rolling average for vaccine doses:
df_main <- df_main %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(vaccine_doses_7d_average= rollapply(daily_vaccine_doses,
                                             FUN = mean,
                                             width = 7,
                                             align = 'right',
                                             fill = NA)) %>% 
  ungroup()


key_states <- c('California',
                'New York',
                'North Dakota',
                'Florida',
                'Oregon',
                'Utah',
                'Texas')

for(st in key_states){
  ggsave(plot = plot_state_indicators(st),
         file = paste0('./exploration_visuals/07_state_metrics_over_time_',str_remove_all(str_to_lower(st), " "),'.png'),
         width = 30, height = 20, units = 'cm')
}

