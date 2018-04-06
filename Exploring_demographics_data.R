library(dplyr)

############
# FP course
############
fp_survey_raw <- read.csv("data/demographic_data/FunctionalProgramming.csv")
str(fp_survey_raw)

# Keep only the survey data for students whose posts were hand-coded
ann_posts <- readRDS("data/pre_processed/all_annotated_posts.RData")
fp_stud_id <- ann_posts %>%
  filter(course=="FP") %>%
  select(student_id) %>%
  unlist() %>%
  as.integer() %>%
  unique()

fp_survey <- fp_survey_raw %>%
  select(-X) %>%
  filter(User.ID %in% fp_stud_id)

# remove variables that are no longer needed
remove(fp_survey_raw)

# Examine the survey data
summary(fp_survey)
# large proportion of NA values for all attributes...

# Check if it is any better for students whose (code) sequences
# were clustered (we're primarily interested in their demographics)
fp_seq <- readRDS("data/pre_processed/FP_sequences_df.RData")
fp_seq_survey <- fp_survey %>%
  filter(User.ID %in% fp_seq$student_id)

# Examine survey data for this subset
summary(fp_seq_survey)
# still a large proportion of NAs...
# There are also empty string values, which are essentially NAs
apply(fp_seq_survey[,-1], 2,
      function(x) sum(trimws(x)=="", na.rm = TRUE))
# In addition, 'None' is present as a value for gender, level_of_education
# Check how frequent these 'None's are
apply(fp_seq_survey[,-1], 2, function(x) sum(x=="None", na.rm = TRUE))
# 41

# turn empty strings and 'None's into NAs
fp_seq_survey <- as.data.frame(apply(fp_seq_survey[,-1], 2,
                                     function(x) ifelse(x=="" | x == "None", NA, x)), 
                                  stringsAsFactors = FALSE)
fp_seq_survey$student_id <- fp_seq$student_id
fp_seq_survey <- fp_seq_survey[,c(13, 1:12)]

# Compute the proportion of NAs for each attribute
stud_seq_count <- nrow(fp_seq_survey)
na_prop <- apply(fp_seq_survey[,-1], 2,
                 function(x) round(sum(is.na(x))/stud_seq_count, digits = 2))
sort(na_prop, decreasing = TRUE)
# IndustryPre, YearsExperiencePre, JobPre: 61-64% of NAs
# NationalityPre, EduLevelPre, country, EduBackgroundPre, OccupationPre, EduLevelPre: 39-49% of NAs
# year_of_birth, gender, level_of_education: 21-26% of NAs

# Examine values for each attribute
apply(fp_seq_survey[,-1], 2,
      function(x) unique(x))


##############
# Solar course
##############
solar_survey_raw <- read.csv("data/demographic_data/SolarEnergy.csv")
str(solar_survey_raw)

# Keep only the survey data for students whose posts were hand-coded
solar_stud_id <- ann_posts %>%
  filter(course=="Solar") %>%
  select(student_id) %>%
  unlist() %>%
  as.integer() %>%
  unique()

solar_survey <- solar_survey_raw %>%
  select(-X) %>%
  filter(User.ID %in% solar_stud_id)

# remove variables that are no longer needed
remove(solar_survey_raw)

# Examine the survey data
summary(solar_survey)
# NAs present, but not nearly as much as in the case of the FP course

# Check if it is any better for students whose (code) sequences
# were clustered (we're primarily interested in their demographics)
solar_seq <- readRDS("data/pre_processed/Solar_sequences_df.RData")
solar_seq_survey <- solar_survey %>%
  filter(User.ID %in% solar_seq$student_id)
# examine survey data for this subset
summary(solar_seq_survey)
# there are still NAs, but not that many...
# There are also empty string values, which are essentially NAs
apply(solar_seq_survey[,-1], 2,
      function(x) sum(trimws(x)=="", na.rm = TRUE))

# In addition, 'None' is present as a value for gender, year_of_birth, level_of_education
# Check how frequent these 'None's are
apply(solar_seq_survey[,-1], 2, function(x) sum(x=="None", na.rm = TRUE))
# not negligible...

# turn empty strings and 'None's into NAs
solar_seq_survey <- as.data.frame(apply(solar_seq_survey[,-1], 2,
                                        function(x) ifelse(x=="" | x == "None", NA, x)), 
                                  stringsAsFactors = FALSE)
solar_seq_survey$student_id <- solar_seq$student_id
solar_seq_survey <- solar_seq_survey[,c(11, 1:10)]

# compute the proportion of NAs for each attribute
solar_na_prop <- apply(solar_seq_survey[,-1], 2,
                       function(x) round(sum(is.na(x))/nrow(solar_seq_survey), digits = 2))
sort(solar_na_prop, decreasing = TRUE)
# BackgroundPre, year_of_birth, gender, level_of_education: 27% of NAs; 
# 17% of NAs for all other attributes

# Examine values for each attribute
apply(solar_seq_survey[,-1], 2,
      function(x) unique(x))


##############
# Water course
##############
h2o_survey_raw <- read.csv("data/demographic_data/WaterTreatment.csv")
str(h2o_survey_raw)

# Keep only the survey data for students whose posts were hand-coded
h2o_stud_id <- ann_posts %>%
  filter(course=="Water") %>%
  select(student_id) %>%
  unlist() %>%
  as.integer() %>%
  unique()

h2o_survey <- h2o_survey_raw %>%
  select(-X) %>%
  filter(User.ID %in% h2o_stud_id)

# remove variables that are no longer needed
remove(h2o_survey_raw)

# Examine the survey data
summary(h2o_survey)
# NAs present, but not as much as in the case of the FP course

# Check if it is any better for students whose (code) sequences
# were clustered (we're primarily interested in their demographics)
h2o_seq <- readRDS("data/pre_processed/h2o_sequences_df.RData")
h2o_seq_survey <- h2o_survey %>%
  filter(User.ID %in% h2o_seq$student_id)

# Examine survey data for this subset
summary(h2o_seq_survey)
# there are still NAs, but not that many...
# There are also empty string values, as well as 'None' string, both of which are 
# essentially NAs
# Check how frequent empty strings are
apply(h2o_seq_survey[,-1], 2,
      function(x) sum(trimws(x)=="", na.rm = TRUE))
# Check also how frequent 'None's are
apply(h2o_seq_survey[,-1], 2, function(x) sum(x=="None", na.rm = TRUE))
# present for a coupe of attributes...

# Turn empty strings and 'None's into NAs
h2o_seq_survey <- as.data.frame(apply(h2o_seq_survey[,-1], 2,
                                      function(x) ifelse(x=="" | x == "None", NA, x)), 
                                  stringsAsFactors = FALSE)
h2o_seq_survey$student_id <- h2o_seq$student_id
h2o_seq_survey <- h2o_seq_survey[,c(11, 1:10)]

# compute the proportion of NAs for each attribute
h2o_na_prop <- apply(h2o_seq_survey[,-1], 2,
                     function(x) round(sum(is.na(x))/nrow(h2o_seq_survey), digits = 2))
sort(h2o_na_prop, decreasing = TRUE)
# BackgroundPre, year_of_birth, gender, level_of_education: 25-27% of NAs; 
# 18-19% of NAs for all other attributes

# Examine values for each attribute
apply(h2o_seq_survey[,-1], 2,
      function(x) unique(x))


##############
# Excel course
##############
ex_survey_raw <- read.csv("data/demographic_data/DataAnalysis.csv")
str(ex_survey_raw)

# Keep only the survey data for students whose posts were hand-coded
ex_stud_id <- ann_posts %>%
  filter(course=="Excel") %>%
  select(student_id) %>%
  unlist() %>%
  as.integer() %>%
  unique()

ex_survey <- ex_survey_raw %>%
  select(-X) %>%
  filter(User.ID %in% ex_stud_id)

# remove variables that are no longer needed
remove(ex_survey_raw)

# Examine the survey data
summary(ex_survey)
# Large proportion of NAs...

# Check if it is any better for students whose (code) sequences
# were clustered (we're primarily interested in their demographics)
ex_seq <- readRDS("data/pre_processed/excel_sequences_df.RData")
ex_seq_survey <- ex_survey %>%
  filter(User.ID %in% ex_seq$student_id)

# Examine survey data for this subset
summary(ex_seq_survey)
# There is still a large proportion of NAs...
# There are also empty string values, as well as 'None' string, both of which are 
# essentially NAs
# Check how frequent empty strings are
apply(ex_seq_survey[,-1], 2,
      function(x) sum(trimws(x)=="", na.rm = TRUE))
# Check also how frequent 'None's are
apply(ex_seq_survey[,-1], 2, function(x) sum(x=="None", na.rm = TRUE))
# present only for two attributes...

# Turn empty strings and 'None's into NAs
ex_seq_survey <- as.data.frame(apply(ex_seq_survey[,-1], 2,
                                      function(x) ifelse(x=="" | x == "None", NA, x)), 
                                stringsAsFactors = FALSE)
ex_seq_survey$student_id <- ex_seq$student_id
ex_seq_survey <- ex_seq_survey[,c(13, 1:12)]

# compute the proportion of NAs for each attribute
ex_na_prop <- apply(ex_seq_survey[,-1], 2,
                     function(x) round(sum(is.na(x))/nrow(ex_seq_survey), digits = 2))
sort(ex_na_prop, decreasing = TRUE)
# country, YearsExperiencePre, IndustryPre, JobPre: 70-76% of NAs; 
# NationalityPre, EduLevelPre, EduBackgroundPre, OccupationPr: 52-59% of NAs
# year_of_birth, gender, level_of_education: 12-15% of NAs

# Examine values for each attribute
apply(ex_seq_survey[,-1], 2,
      function(x) unique(x))
                                  