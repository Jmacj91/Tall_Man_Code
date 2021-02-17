#load libraries
library(tidyverse)
library(tidycensus)
library(tigris)

census_api_key(Your_Key_Goes_Here, overwrite = T, install = T)
acsvars <- load_variables(2015, "acs5", cache = TRUE)

#Selecting the codes for all age ranges by sex from the acsvars dataset
agesex <- c('B01001_001', 'B01001_002', 'B01001_003', 'B01001_004',
            'B01001_005', 'B01001_006', 'B01001_007', 'B01001_008',
            'B01001_009', 'B01001_010', 'B01001_011', 'B01001_012',
            'B01001_013', 'B01001_014', 'B01001_015', 'B01001_016',
            'B01001_017', 'B01001_018', 'B01001_019', 'B01001_020',
            'B01001_021', 'B01001_022', 'B01001_023', 'B01001_024',
            'B01001_025', 'B01001_026', 'B01001_027', 'B01001_028',
            'B01001_029', 'B01001_030', 'B01001_031', 'B01001_032',
            'B01001_033', 'B01001_034', 'B01001_035', 'B01001_036',
            'B01001_037', 'B01001_038', 'B01001_039', 'B01001_040',
            'B01001_041', 'B01001_042', 'B01001_043', 'B01001_044',
            'B01001_045', 'B01001_046', 'B01001_047', 'B01001_048',
            'B01001_049')

#dividing age ranges into categories
baby <-  c('B01001_003','B01001_027')
young <- c('B01001_003', 'B01001_004','B01001_005', 'B01001_006', 'B01001_007', 'B01001_008', 'B01001_009', 'B01001_010',
           'B01001_027', 'B01001_028','B01001_029', 'B01001_030', 'B01001_031', 'B01001_032', 'B01001_033', 'B01001_034')
mid  <-  c('B01001_011', 'B01001_012','B01001_013', 'B01001_014', 'B01001_035', 'B01001_036', 'B01001_037', 'B01001_038')
older <- c('B01001_015', 'B01001_016','B01001_017', 'B01001_018', 'B01001_019', 'B01001_039', 'B01001_040', 'B01001_041', 
           'B01001_042', 'B01001_043')
old  <-  c('B01001_020', 'B01001_021','B01001_022', 'B01001_023', 'B01001_024','B01001_025',
           'B01001_044', 'B01001_045','B01001_046', 'B01001_047', 'B01001_048','B01001_049') 

#loading the dataset for the ACS 5 year estimates of age by census tract

mdacs <- get_acs(geography = "tract",
                 #the list of variables we wanted earlier
                 variables = agesex,
                 #MD is the 24th state
                 state = 24,
                 #Baltimore is county code 510
                 county = 510,
                 #this would be for 2011-2015
                 year = 2015,
                 #mark T if you want to do mapping later
                 geometry = F,
                 #sets the type of survey you want
                 survey = "acs5") %>%
  #this joins it with the list of variables so you get the concept and label in case 
  #you want it to validate things worked. Not totally necessary but nice to see.
  left_join(.,acsvars, by = c("variable" = "name")) %>%
  #then you can remove it.
  select(.,-c(1,5:7)) %>%
  #now you can categorize by the groups we created above.
  mutate(Age_Group = 
           ifelse(.$variable %in% baby, "Population Under 5",
                  ifelse(.$variable %in% baby, "Population Under 5",
                         ifelse(.$variable %in% young, "Population Under 25",
                                ifelse(.$variable %in% mid, "Population 25 to 44",
                                       ifelse(.$variable %in% older, "Population 45 to 64",
                                              ifelse(.$variable %in% old, "Population 65 and Older",NA
                                              ))))))) %>%
  #get rid of anything that's not an age group
  filter(.,is.na(Age_Group)==F) %>%
  #filter and then arrange the order
  select(.,-c(2)) %>%
  select(c(1,3,2)) %>%
  #now add together all the age groups so you can see the population of each group
  group_by('Census Tract' =  NAME,'Age_Group' = Age_Group) %>%
  summarise(Population = sum(estimate)) %>%
  #this then creates columns for each age group so you can see it by census group
  pivot_wider(.,names_from = Age_Group,values_from = Population) %>%
  #arrange the columns correctly, ungroup, and then add a total column
  select(c(1,6,5,2,3,4)) %>%
  ungroup() %>%
  mutate(`Total Population` = rowSums(.[,2:6])) 

##The result of this is a table of every census tract with the population by age. 
##You could easily do a similar analysis of population demographics, poverty rate, 
##or any other factor you're hoping to analyze. 
