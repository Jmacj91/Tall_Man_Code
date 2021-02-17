## Using the explainer from Jennifer Huck's GitHub
## https://jennhuck.github.io/workshops/tidycensus.html

## Load packages----------------------------------------------------------------
library(tidyverse) 
library(tidycensus) 
library(tigris)
library(censusapi) 

#This document shows how to load a basic Census or ACS data frame using the Census
#API and the tidycensus package. There's a lot more you can do if you want to map
#or do analytics in R, but here's a good place to start.

## API--------------------------------------------------------------------------

# when install = TRUE, you don't have to keep using this line in your scripts
census_api_key("YOUR API KEY GOES HERE", overwrite = T, install = TRUE)

## Review documentation---------------------------------------------------------
?get_decennial #1990, 2000, 2010 available, default is 2010
?get_acs # defaults: year: 2017 (endyear for 5-year ACS); survey: 5-year; 
  # moe_level: 90
?load_variables #dataset: One of "sf1", "sf3", "acs1", "acs3", "acs5", 
  #"acs1/profile", "acs3/profile, "acs5/profile", "acs1/subject", "acs3/subject",
  # or "acs5/subject".

## Load Variables---------------------------------------------------------------
# View all the avialable variables for 5-year ACS 2013-2017
acsvars15 <- load_variables(year = 2015, dataset = "acs5", cache = TRUE)
View(acsvars15)

## Look for tables--------------------------------------------------------------
# Now you need some table IDs

# Use the search bar with the magnifying glass in the spreadsheet viewer or
# Use a secondary source like Census Reporter or data.census.gov

# Or try grep
v17_profile[grep(x = v17_profile$label, "Median household"), c("name", "label")]

v17[grep(x = v17$label, "Median household"), c("name", "label")] %>% 
  print(n = 100)

# Or filter the variables with dplyr's filter and stringr's str_detect
poverty <- filter(v17, str_detect(label, "poverty"))
View(poverty)

B17010 <- filter(v17, str_detect(name, "B17010"))
View(B17010)


## Decennial Census-------------------------------------------------------------

# Pull Decennial Census population counts by state
state_pop <- get_decennial(geography = "state", 
                           variables = "P001001") # default is 2010
state_pop #evaluate, notice number of rows

# Save random rows from a table, obviously more useful when you have a lot of observations
# Test your code on your sample
state_pop_sample_n <- sample_n(state_pop, 10) #retrieve 10 random rows
state_pop_sample_frac <- sample_frac(state_pop, .1) #retrieve 10% of rows

# Use ggplot to visualize and reorder
state_pop %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()


## ACS--------------------------------------------------------------------------

# Pull income estimates by state with ACS 2013-2017 5-year survey

state_income <- get_acs(geography = "state",
                        variables = "B19013_001") 
state_income

## Exercise #1------------------------------------------------------------------

# Pull population counts by state for the year 2000. Find and replace "___" to make code run.

state_pop_00 <- get_decennial(geography = "state", 
                              variables = "P001001"#,
                             # ___ = ___
                              ) 
state_pop_00


## Lookup geography codes with tigris package-----------------------------------

# Look up FIPS codes for specific geographies
lookup_code("Maryland", "Baltimore") 

# List all counties in Virginia, three ways - they all work the same
list_counties("Maryland") # use state name
list_counties("MD") # use postal code
list_counties("24") # use FIPS code

## Geographic subsets for ACS---------------------------------------------------

# Pull the 2013-2017 ACS 5-year estimates for family poverty in the Baltimore MSA

# by census tract
fp_cv_tract <- get_acs(geography = "tract", #geography is tract
                      variables = c(family_poverty = "B17010_002"), #pass a named vector in the variables argument, for human readability
                      state = "MD", # you are required to include a state to pull tracts
                      county = "510") # you can optionally include a county to pull tracts
head(fp_cv_tract)

# by block group
fp_cv_bg <- get_acs(geography = "block group", #geography is block group
                          variables = c(family_poverty = "B17010_002"),
                          state = "MD",
                          county ="510")

# Pull multiple counties to create the Baltimore MSA
fp_msa_tract <- get_acs(geography = "tract", 
                           variables = c(family_poverty = "B17010_002"), 
                           state = "24", 
                           county = c("Baltimore city","Baltimore County","Howard","Anne Arundel","Harford","Carroll"), #concatenate county codes
                           geometry = T) # returns an sf tibble with simple feature geometry, useful for mapping later

# Median household income, check for missing estimates
hhi_msa_tract <- get_acs(geography = "tract", 
                        variables = c(family_poverty = "DP03_0062E"), 
                        state = "24", 
                        county = c("Baltimore city","Baltimore County","Howard","Anne Arundel","Harford","Carroll")
                        )
filter(hhi_msa_tract, is.na(estimate))

## Visualizations---------------------------------------------------------------

# Create a map with ggplot with our MSA geometry
# geom_sf useful here
fp_msa_tract %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() +
  ggtitle("Family Poverty - Baltimore MSA, 2017")

# Visualize with a plot
# geom_errorbarh useful here (see also geom_errorbar)
fp_cv_tract %>%
  mutate(NAME = gsub(", Baltimore city, Maryland", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 1) +
  labs(title = "Poverty by Census Tract In Baltimore",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

## censusapi package------------------------------------------------------------
# Let's look at the censusapi package

## Install API------------------------------------------------------------------
# different package means we need to install the API again 
# if you are on a shared computer, use censusapi::getcensus key argument instead

# add key to .Renviron, only need to do once
Sys.setenv(CENSUS_KEY="e598bc95dd62b82fa87e928529f2f6072c7dfa3e")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

## View of a list of every available endpoint-----------------------------------
# Includes useful metadata such as 'name'
apis <- listCensusApis()
View(apis)
# Type in 'health' in the magnifying glass search bar, note 'name' field

## Small Area Health Insurance Estimates: variables and geographies-------------
# Get unisured rates by income group using the Small Area Health Insurance Estimates endpoint
# which provides detailed annual and county-level estimates of health insurance rates

# retrieve variable metadata
sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", 
                                 type = "variables")
View(sahie_vars)

# retrieve geography metadata
listCensusMetadata(name = "timeseries/healthins/sahie", 
                   type = "geography")
# 3 geographic levels: US, County, State

# Let's call these variables, as seen in sahie_vars:
# IPRCAT: Income Poverty Ratio Category
# IPR_DESC: Income Poverty Ratio Category Description
# PCTUI_PT: Percent Uninsured in Demographic Group for Selected Income Range, Estimate
# NAME: Name of the geography returned (e.g. state or county name)

# view uninsured rate by income group at the national level for 2017
getCensus(name = "timeseries/healthins/sahie",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
          region = "us:*", 
          time = 2017)
# FYI - Income <= 138% of Poverty is eligible for Medicaid

# view uninsured rate by income group at state-level for Virginia in 2017
getCensus(name = "timeseries/healthins/sahie",
                          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                          region = "state:24", 
                          time = 2017)

# view Charlottesville
getCensus(name = "timeseries/healthins/sahie",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
          region = "county:510", 
          regionin = "state:24", 
          time = 2017)

# save uninsured rate by income group at county-level for Virginia in 2017
sahie_counties <- getCensus(name = "timeseries/healthins/sahie",
                            vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                            region = "county:*", 
                            regionin = "state:24", 
                            time = 2017)
head(sahie_counties, n=12L)

## Using tidy tools in combination with censusapi-------------------------------

# using tidyverse tools, create a ratio variable (<= 138% of Poverty / All Incomes), 
# arrange by largest ratio of low-income to all-income uninsured
sahie_counties_ratio <- sahie_counties %>% 
  filter(IPRCAT == 3 | IPRCAT == 0) %>% 
  select(-IPRCAT) %>% 
  spread(key = IPR_DESC, value = PCTUI_PT, convert = TRUE) %>% 
  rename(poverty_138 = "<= 138% of Poverty", all_incomes = "All Incomes") %>% 
  mutate(ratio = poverty_138 / all_incomes) %>% 
  arrange(desc(ratio))

## Using tigris to map censusapi variables--------------------------------------

# for more on sf in tigris see https://walkerke.github.io/2017/05/tigris-v05/
options(tigris_class = "sf")

# download US counties shapefile into R, optionally subset by state
md_co <- counties(state = "Maryland", cb = TRUE)
head(md_co)

# join "uninsured ratio" data frame to VA counties sf dataframe using left_join 
sahie_counties_ratio_geo <-  sahie_counties_ratio %>% 
  left_join(md_co, c("county" = "COUNTYFP"))

# map with ggplot and geom_sf()
sahie_counties_ratio_geo %>%
  ggplot(aes(fill = ratio, color = ratio, geometry = geometry)) + 
  geom_sf() +
  ggtitle("Ratio of low-income to all-income uninsured - Maryland Counties, 2017")

## Recreate Family Poverty estimates using censusapi, compare to tidycensus-----

# note available geographies
acs5_geo <- listCensusMetadata(name = "acs/acs5", 
                   type = "geography",
                   vintage = 2017)

# use 'group' argument to call a table
acs5_B17010 <- listCensusMetadata(name = "acs/acs5", 
                            vintage = 2017,
                            type = "variables",
                            group = "B17010")
View(acs5_B17010)

fp_cv_tract_ca <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("NAME", "B17010_002E", "B17010_002EA", "B17010_002M"), 
                        region = "tract:*", 
                        regionin = "state:24+county:510")
head(fp_cv_tract_ca)

# compare to tidycensus version, ~line 109
 fp_cv_tract <- get_acs(geography = "tract",
                           variables = c(family_poverty = "B17010_002"),
                           state = "24", 
                           county = "510")
head(fp_cv_tract)

