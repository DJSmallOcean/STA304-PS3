#### Data Cleaning ####

#### Workspace setup ####
library(haven)
library(tidyverse)

#### Cleaing Survey data ####
# Read in the raw data
raw_survey_data <- read_dta("ns20200625.dta")
# Add the labels
raw_survey_data <- labelled::to_factor(raw_survey_data)
# Select some useful variables
reduced_survey_data <- 
  raw_survey_data %>% 
  select(vote_2020,
         vote_intention,
         registration,
         age,
         gender,
         education,
         state,
         household_income,
         race_ethnicity)
# Change Data type
reduced_survey_data$age<-as.numeric(reduced_survey_data$age)
# Select people are both registered & intented to vote for either Trump or Biden; Assume people will vote unless they specify no
survey_data<-reduced_survey_data %>% 
  filter(registration=="Registered"&
           vote_intention!="No, I am not eligible to vote"&
           vote_intention!="No, I will not vote but I am eligible"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
  )
# Drop NAs
survey_data<-na.omit(survey_data)
rm(raw_survey_data,reduced_survey_data)


#### Cleaning Census data ####
# Read in the raw data
raw_census_data <- read_dta("usa_00002.dta")
# Add the labels
raw_census_data <- labelled::to_factor(raw_census_data)
# Select some useful variables
reduced_census_data <- 
  raw_census_data %>% 
  select(perwt,
         citizen,
         age,
         sex, 
         educd,
         stateicp,
         hhincome,
         race
  )
# Change data type
reduced_census_data$age<-as.numeric(reduced_census_data$age)
# Select people who are eligible to vote
census_data<-reduced_census_data %>% filter(age>=18 & (citizen=="naturalized citizen"|citizen=="born abroad of american parents"))

# Drop NAs
census_data$hhincome<-ifelse(census_data$hhincome==9999999,
                                      NaN,census_data$hhincome)
census_data<-na.omit(census_data)
rm(raw_census_data,reduced_census_data)


#### Data type mapping between survey & census data ####
#Create Age groups
survey_data<-survey_data %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80'
  )) 
census_data<-census_data %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <=  35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80' 
  )) 

unique(census_data$agegroup)
unique(survey_data$agegroup)


# Mapping sex variable
census_data$sex<-ifelse(census_data$sex=="female","Female","Male")
# Rename gender variable to sex in census data
census_data<-rename(census_data,gender=sex)

unique(census_data$gender)
unique(survey_data$gender)


# Mapping education variable
# Assume Other post high school vocational training = High school graduate 
# Completed some graduate, but no degree = College degree
# professional degree beyond a bachelor's degree = College degree

# Survey data
survey_data$education[survey_data$education=="Other post high school vocational training"]<-"High school graduate"
survey_data$education[survey_data$education=="Completed some graduate, but no degree"]<-"College Degree (such as B.A., B.S.)"
# Census data
lowleveledu<-c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade4to8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade9to12<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
highschoolgra<-c("ged or alternative credential","regular high school diploma")
somecollege<-c("some college, but less than 1 year",
               "1 or more years of college credit, no degree")
census_data<-census_data %>% 
  mutate(educd2 = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd %in% lowleveledu ~"3rd Grade or less",
                            educd %in% grade4to8~"Middle School - Grades 4 - 8",
                            educd %in% grade9to12~"Completed some high school",
                            educd %in% highschoolgra~"High school graduate",
                            educd %in% somecollege~"Completed some college, but no degree",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="doctoral degree"~'Doctorate degree'
  )) 
census_data<-rename(census_data,education=educd2)
census_data$educd<-NULL

unique(census_data$education)
unique(survey_data$education)


# Mapping State variable
census_data<-census_data %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC")) 
census_data$stateicp<-NULL

unique(census_data$state)
unique(survey_data$state)

# Mapping household income variable
census_data<-census_data %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 
census_data$hhincome<-NULL

unique(census_data$household_income)
unique(survey_data$household_income)

# Mapping race variable
otherasian<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)","Asian (Filipino)",
              "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")
#survey data
survey_data<-survey_data %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% otherasian ~"other asian or pacific islander",
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity=="Other race "~"Other race"
  )) 
survey_data$race_ethnicity<-NULL
#census data
census_data<-census_data %>% 
  mutate(race2 = case_when(race=="white"~"White",
                           race=="chinese"~"Chinese",
                           race=="black/african american/negro"~"Black, or African American",
                           race=="two major races"~"Other race",
                           race=="other race, nec"~"Other race",
                           race=="japanese"~"Japanese",
                           race=="american indian or alaska native"~"American Indian or Alaska Native",
                           race=="three or more major races"~"Other race",
                           race=="other asian or pacific islander"~"other asian or pacific islander"
  )) 
census_data$race<-census_data$race2
census_data$race2<-NULL

unique(census_data$race)
unique(survey_data$race)


# Saving the survey & census data as a csv file in my working directory
write_csv(survey_data, "survey_data.csv")
write_csv(census_data, "census_data.csv")
