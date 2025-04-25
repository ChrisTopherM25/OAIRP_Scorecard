
setwd("")
#### College API Demo Sheet OAIRP ####
## The goal of this document is to use the college scorecard API to retrieve  
## select data from all universities within Ohio, Indiana and Kentucky.
## We are most interested in comparing universities by size, institutional 
## control, cost and other rates.

#### Setup ####
# Load packages
library(tidyverse) # All the tidy things
library(jsonlite)  # Converting json data into data frames
library(magrittr)  # Extracting items from list objects using piping grammar
library(ggplot2)  # Used for building visualizations
library(dplyr)   # Used for data manipulation 

#### Defining the API call URL #### 
## To identify the criteria wanted to specify,
## the scorecard data dictionary is available at:
## https://collegescorecard.ed.gov/assets/CollegeScorecardDataDictionary.xlsx

## Defining the API endpoint. Provided by the API documentation on the college
## scorecard website: https://collegescorecard.ed.gov/data/documentation/
# This defines the base URL (endpoint) path for the scorecard API. 
ScoreCend <- "https://api.data.gov/ed/collegescorecard/v1/schools.json?"

### API Key ###
## An API key is needed to interact with the scorecard API.
## Users can apply for a key at https://collegescorecard.ed.gov/data/documentation/

# Defining the API key as a parameter
api_key <- "api_key="


### Defining Field Parameters ###
## Only request the necessary fields, as a courtesy to the host.
## Without this option parameter, requests will retrieve every 
## field available for every query request


## Function for requesting multiple pages from the Scorecard API (Lists > 100)

#Generating the URL request
sc_api_GET_url <- 
  function(api_key, school_name, state, page) {
    # Define the endpoint
    sc_endpoint <- "https://api.data.gov/ed/collegescorecard/v1/schools.json?"
    # Define your API key query parameter
    api_key <- paste("api_key=",api_key, sep = "")
    # Define field parameters
    
    ## The following is the notation used to identify fields: 
    ## year.dev_category.dev_friendly_name
    ## For example, if you wanted to retrieve the 'name' field from the 'school'
    ## dev_category, you would specify: school.name
    
    ## The year classification is necessary for fields that iterate annually.
    ## For example, if you wanted to retrieve the 'size' category from the 'school'
    ## dev_category but only for the year 2019, you would specify: 2019.student.size
    ## else for the most recent year lastest.student.size works.
    fields <- paste("fields=id",
                    "school.name",
                    "latest.student.size",
                    "school.ownership",
                    "location.lat",
                    "location.lon",
                    "school.state",
                    "school.locale",
                    "school.faculty_salary",
                    "school.institutional_characteristics.level",
                    "school.main_campus",
                    "latest.admissions.admission_rate.overall",
                    "latest.admissions.sat_scores.midpoint.critical_reading",
                    "latest.admissions.sat_scores.midpoint.math",
                    "latest.admissions.sat_scores.midpoint.writing",
                    "latest.admissions.sat_scores.average.overall",
                    "latest.admissions.act_scores.midpoint.english",
                    "latest.admissions.act_scores.midpoint.math",
                    "latest.admissions.act_scores.midpoint.writing",
                    "latest.admissions.act_scores.midpoint.cumulative",
                    "latest.earnings.10_yrs_after_entry.working_not_enrolled.overall",
                    "latest.earnings.10_yrs_after_entry.working_not_enrolled.mean_earnings",
                    "latest.earnings.10_yrs_after_entry.female_students",
                    "latest.earnings.10_yrs_after_entry.male_students",
                    "latest.earnings.10_yrs_after_entry.mean_earnings.female_students",
                    "latest.earnings.10_yrs_after_entry.mean_earnings.male_students",
                    "latest.student.demographics.race_ethnicity.white",
                    "latest.student.demographics.race_ethnicity.black",
                    "latest.student.demographics.race_ethnicity.hispanic",
                    "latest.student.demographics.race_ethnicity.asian",
                    "latest.student.demographics.race_ethnicity.aian",
                    "latest.student.demographics.race_ethnicity.nhpi",
                    "latest.student.demographics.race_ethnicity.two_or_more",
                    "latest.student.demographics.race_ethnicity.non_resident_alien",
                    "latest.student.demographics.race_ethnicity.unknown",
                    "latest.student.demographics.avg_family_income",
                    "latest.cost.attendance.academic_year",
                    "latest.cost.tuition.in_state",
                    "latest.cost.tuition.out_of_state",
                    "latest.student.retention_rate.four_year.full_time",
                    "latest.student.retention_rate.four_year.part_time",
                    "latest.completion.completion_rate_4yr_150nt",
                    sep = ",")
    
    ### Filtering results ###
    school_name <- paste("school.name=",school_name, sep = "")
    # Filter to schools only within the tri-state area (OH, KY, IN)
    state <- paste("school.state=",state, sep = "")
    # Define the number of results per page
    per_page <- "per_page=100"
    # Page numbers start at 0.
    page_param <- paste("page=", page, sep = "")
    # Creating your final API call URL #
    sc_api_GET <- 
      paste(sc_endpoint,api_key,school_name,fields,state,per_page,page_param,sep = "&")
    return(sc_api_GET)
  }


scorecard_df <- 
  function(api_key, school_name, state){
    all_results <- NULL
    page <- 0
    per_pageC <- 100
    results_count <- per_pageC
    
    while (results_count == per_pageC) {
      print(paste("Fetching page:", page)) # Added print statement
      sc_api_GET <- sc_api_GET_url(api_key, school_name, state, page)
      
      # Extract the API call results to a data frame and reorder the vectors as needed
      # The eval() function below is used to evaluate the URL string
      # before passing the result to fromJSON() which is used to import JSON data into R.
      scorecard_GET_df <- 
        sc_api_GET %>% 
        eval() %>% 
        fromJSON() %>% 
        use_series(results) %>% 
        relocate(id,school.name) 
      
      results_count <- nrow(scorecard_GET_df)
      all_results <- rbind(all_results, scorecard_GET_df)
      
      if (results_count < 100) { #Checking for pages to request beyond first page
        return(all_results)
      } else {
        page <- page + 1
      }
      Sys.sleep(1)
    }
  }

# When requesting data, import your API Key, the filter on University Name and then 
# the States your are looking to request

triState_universities <- scorecard_df("your_API_KEY","university","OH,KY,IN")


## Cleaning and preparing the requested data for use

## Rename columns based on what s found in the Scorecard Code book
colnames(triState_universities) <- c(
  "ID", "School","Student_Size","Admission_Rate",
  "SAT_Reading_Avg","SAT_Math_Avg","SAT_Writing_Avg","SAT_Overall_Avg",
  "ACT_English_Avg","ACT_Math_Avg","ACT_Writing_Avg","ACT_Cumulative_Avg",
  "Count_Working_Students","Mean_Earnings","F_Count_Working_Students","M_Count_Working_Students",
  "F_Mean_Earnings","M_Mean_Earnings","Share_White_Students","Share_Black_Students",
  "Share_Hispanic_Students","Share_Asian_Students","Share_Aian_Students","Share_Nhpi_Students",
  "Share_two_or_more_Students","Share_Alien_Students","Share_Unknown_Students","FAMINC",
  "COSTT4_A","TUITIONFEE_IN","TUITIONFEE_OUT","RET_FT4","RET_PT4",
  "Completion_Rate","CONTROL","STABBR","LOCALE","AVGFACSAL","Institutional_Type","Main_Campus","LATITUDE","LONGITUDE")

triState_universities <- triState_universities %>%
  mutate(Main_Campus = recode(Main_Campus,
                              `1` = "Main_Campus",
                              `0` = "Not main campus"))

triState_universities <- triState_universities %>%
  mutate(CONTROL = recode(CONTROL,
                          `1` = "Public",
                          `2` = "Private nonprofit",
                          `3` = "Private for-profit"))

triState_universities <- triState_universities %>%
  mutate(Institutional_Type = recode(Institutional_Type,
                                     `1` = "4-year",
                                     `2` = "2-year",
                                     `3` = "Less-than-2-year"))

triState_universities <- triState_universities %>%
  mutate(LOCALE = recode(LOCALE,
                                     `11` = "City",
                                     `12` = "City",
                                     `13` = "City",
                                     `21` = "Suburb",
                                     `22` = "Suburb",
                                     `23` = "Suburb",
                                     `31` = "Town",
                                     `32` = "Town",
                                     `33` = "Town",
                                     `41` = "Rural",
                                     `42` = "Rural",
                                     `43` = "Rural"))

str(triState_universities)                                




### Other Applications ###
#highlight Xavier in all Graphics
highlight <- triState_universities %>% filter(ID == 206622)

#How do Tri-State area schools cost of tuition compare to one another?
triState_universities %>%
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  group_by(STABBR) %>%
  summarise(`Average Cost` = mean(COSTT4_A, na.rm=TRUE)) %>%
  ggplot(aes(x = STABBR, y = `Average Cost`)) +
  geom_col() +
  geom_point(data = highlight, aes(x = STABBR, y = COSTT4_A), color = "red", size = 4)

# The distribution of COST4_A variable 
triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = COSTT4_A)) +
  geom_histogram(bins = 1000) +
  geom_vline(data = highlight, aes(xintercept = COSTT4_A), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of University Costs (COSTT4_A)")

#Counts of Control
triState_universities %>%
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = CONTROL)) + 
  geom_bar() + 
  geom_text(data = highlight, aes(x = CONTROL, y = 1, label = "Xavier"), color = "red", vjust = -1) +
  labs(title = "University Classification")

# Average COSTT4_A for each classification of Control

triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = CONTROL, y = COSTT4_A)) +
  geom_bar(stat = "summary", fun = mean) +
  geom_point(data = highlight, aes(x = CONTROL, y = COSTT4_A), color = "red", size = 4) +
  labs(title = "University Type Average Tuition")

# Illustrate the relationship between FAMINC and COST4_A

triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = FAMINC, y = COSTT4_A)) +
  geom_point(alpha = 0.5) +
  geom_point(data = highlight, aes(x = FAMINC, y = COSTT4_A), color = "red", size = 4) +
  labs(title = "Relationship between Family Income and Cost of Attendance")


# High faculty salaries are partially paid for by high tuition costs

triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = COSTT4_A, y = AVGFACSAL, color = CONTROL)) +
  geom_point(alpha = 0.6) +
  geom_point(data = highlight, aes(x = COSTT4_A, y = AVGFACSAL), color = "black", size = 4) +
  scale_x_log10(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "University Tuition vs Faculty Salary",
       subtitle = "Institution 206622 highlighted in black")


# Are there are more public schools in city and suburban areas and more private schools in towns and rural locales.

triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = CONTROL, fill = CONTROL)) +
  geom_bar() +
  geom_text(data = highlight, aes(x = CONTROL, y = 1, label = "???"), color = "red", size = 6) +
  facet_wrap(~ LOCALE)


# Do Faculty salaries more significantly influence tuition costs at private schools than public schools"

triState_universities %>% 
  filter(STABBR %in% c("OH", "IN", "KY")) %>%
  ggplot(aes(x = AVGFACSAL, y = COSTT4_A)) +
  geom_point(alpha = 0.5) +
  geom_point(data = highlight, aes(x = AVGFACSAL, y = COSTT4_A), color = "red", size = 4) +
  geom_smooth(method = 'lm') +
  facet_wrap(~ CONTROL, nrow = 2) +
  scale_y_log10(labels = scales::dollar) +
  scale_x_continuous(labels = scales::dollar)

