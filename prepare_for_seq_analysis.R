library(tidyverse)

all_ann_posts <- readRDS("data/pre_processed/all_annotated_posts.RData")

# compute the number of posts per student
posts_per_stud <- all_ann_posts %>%
  group_by(course, student_id) %>%
  summarise(n_posts = n())
summary(posts_per_stud$n_posts)

# remove students who had just one post - no sequence can be created for them
stud_one_post <- posts_per_stud$student_id[posts_per_stud$n_posts==1]
length(stud_one_post)
# 2310
length(unique(stud_one_post))
# 2299
# so, there were students who were enrolled in more than one of these courses
# better do the removal for each course separately

############################################
# Examining and cleaning data: Solar course
############################################
solar_post_per_stud <- all_ann_posts %>%
  filter(course == "Solar") %>%
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(solar_post_per_stud$n_posts)

# Find students who had just one post
solar_one_post_stud <- solar_post_per_stud$student_id[solar_post_per_stud$n_posts==1]
length(unique(solar_one_post_stud))
# 693 students with just one post 
# examine the distribution of their posts across the post types
one_post_stud <- all_ann_posts %>% 
  filter(course == "Solar") %>%
  select(-course) %>%
  filter(student_id %in% solar_one_post_stud) 
round(prop.table(summary(one_post_stud$code)), digits = 2)
#   CN   CT  INF META  MIX   SN   ST 
# 0.22 0.36 0.25 0.01 0.05 0.08 0.03  

# Remove students with just one post
solar_ann_posts <- all_ann_posts %>% 
  filter(course == "Solar") %>%
  select(-course) %>%
  filter(!student_id %in% solar_one_post_stud)
length(unique(solar_ann_posts$student_id))
# 648 students with at least 2 posts
# examine now the number of posts per stud
solar_post_per_stud <- solar_ann_posts %>% 
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(solar_post_per_stud$n_posts)
quantile(solar_post_per_stud$n_posts, probs = seq(0.9,1,0.01))
# would have to remove students (i.e. sequences) w/ length above 98th percentile (39)
# UPDATE: remove students (sequences) w/ length above 97th percentile (29)
length(solar_post_per_stud$student_id[solar_post_per_stud$n_posts > 29])
# 18 students - remove them from further sequence analysis and examine their sequences manually?
solar_outliers <- solar_post_per_stud$student_id[solar_post_per_stud$n_posts > 29]

# sort the posts, so that for each student, we have posts in the order 
# they appeared (based on the timestamp)
solar_posts_sorted <- solar_ann_posts %>% arrange(student_id, timestamp)
head(solar_posts_sorted, n = 20)

# remove 'outliers'
solar_posts_sorted <- solar_posts_sorted %>%
  filter(!student_id %in% solar_outliers)
# examine the effect of removal of outliers on the posts count
(nrow(solar_ann_posts) - nrow(solar_posts_sorted))/nrow(solar_ann_posts)
# this led to the removal of 2254 posts, 41% of the original number


#########################################
# Examining and cleaning data: FP course
#########################################
fp_post_per_stud <- all_ann_posts %>%
  filter(course == "FP") %>%
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(fp_post_per_stud$n_posts)
# remove students who had just one post
fp_one_post_stud <- fp_post_per_stud$student_id[fp_post_per_stud$n_posts==1]
length(unique(fp_one_post_stud))
# 371 students with just one post - remove them
fp_ann_posts <- all_ann_posts %>% 
  filter(course == "FP") %>%
  select(-course) %>%
  filter(!student_id %in% fp_one_post_stud)
length(unique(fp_ann_posts$student_id))
# 513 students with at least 2 posts
# examine now the number of posts per stud
fp_post_per_stud <- fp_ann_posts %>% 
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(fp_post_per_stud$n_posts)
quantile(fp_post_per_stud$n_posts, probs = seq(0.9,1,0.01))
# would have to remove students (i.e. sequences) w/ length above 98th percentile (67)
# UPDATE: remove students (sequences) w/ length above 97th percentile (54)
length(fp_post_per_stud$student_id[fp_post_per_stud$n_posts > 54])
# 16 students - remove them from further sequence analysis and examine their sequences manually?
fp_outliers <- fp_post_per_stud$student_id[fp_post_per_stud$n_posts > 54]

# sort the posts, so that for each student, we have posts in the order 
# they appeared (based on the timestamp)
fp_posts_sorted <- fp_ann_posts %>% arrange(student_id, timestamp)
head(fp_posts_sorted, n = 20)

# remove 'outliers'
fp_posts_sorted <- fp_posts_sorted %>%
  filter(!student_id %in% fp_outliers)
# examine the effect of removal of outliers on the posts count
(nrow(fp_ann_posts) - nrow(fp_posts_sorted))/nrow(fp_ann_posts)
# this led to the removal of 3030 posts, 49% of the original number
# Note: it was 16 students - out of 513 - who contributed 51% of posts

############################################
# Examining and cleaning data: Excel course
############################################
ex_post_per_stud <- all_ann_posts %>%
  filter(course == "Excel") %>%
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(ex_post_per_stud$n_posts)
# examine students who had just one post
ex_one_post_stud <- ex_post_per_stud$student_id[ex_post_per_stud$n_posts==1]
length(unique(ex_one_post_stud))
# 518 students with just one post
# examine the distribution of their posts across the post types
one_post_stud <- all_ann_posts %>% 
  filter(course == "Excel") %>%
  select(-course) %>%
  filter(student_id %in% ex_one_post_stud) 
round(prop.table(summary(one_post_stud$code)), digits = 2)
#   CN   CT  INF META  MIX   SN   ST 
# 0.17 0.50 0.11 0.00 0.07 0.10 0.05
# Those with one post only were primarily focused on curriculum-related
# and task-related topics
# Remove students with one post only
ex_ann_posts <- all_ann_posts %>% 
  filter(course == "Excel") %>%
  select(-course) %>%
  filter(!student_id %in% ex_one_post_stud)
length(unique(ex_ann_posts$student_id))
# 519 students with at least 2 posts
# examine now the number of posts per stud
ex_post_per_stud <- ex_ann_posts %>% 
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(ex_post_per_stud$n_posts)
quantile(ex_post_per_stud$n_posts, probs = seq(0.9,1,0.01))
# would have to remove students (i.e. sequences) w/ length above 98th percentile (50)
# UPDATE: as in the other 3 courses, better reduce sequence length even further by 
# taking 97th percentile (37)
length(ex_post_per_stud$student_id[ex_post_per_stud$n_posts > 37])
# 16 students - remove them from further sequence analysis and examine their sequences manually?
ex_outliers <- ex_post_per_stud$student_id[ex_post_per_stud$n_posts > 37]

# sort the posts, so that for each student, we have posts in the order 
# they appeared (based on the timestamp)
ex_posts_sorted <- ex_ann_posts %>% arrange(student_id, timestamp)
head(ex_posts_sorted, n = 20)

# remove 'outliers'
ex_posts_sorted <- ex_posts_sorted %>%
  filter(!student_id %in% ex_outliers)
# examine the effect of removal of outliers on the posts count
(nrow(ex_ann_posts) - nrow(ex_posts_sorted))/nrow(ex_ann_posts)
# this led to the removal of 2074 posts, 44% of the original number


###############
# Water course
###############
h2o_post_per_stud <- all_ann_posts %>%
  filter(course == "Water") %>%
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(h2o_post_per_stud$n_posts)
# examine students who had just one post
h2o_one_post_stud <- h2o_post_per_stud$student_id[h2o_post_per_stud$n_posts==1]
length(unique(h2o_one_post_stud))
# 728 students with just one post
# examine the distribution of their posts across the post types
one_post_stud <- all_ann_posts %>% 
  filter(course == "Water") %>%
  select(-course) %>%
  filter(student_id %in% h2o_one_post_stud) 
round(prop.table(summary(one_post_stud$code)), digits = 2)
#   CN   CT  INF META  MIX   SN   ST 
# 0.40 0.09 0.10 0.00 0.01 0.39 0.02
# Interesting, those who posted only once, posted on non-task topics 
# The pattern is clearly different than the one observed e.g. in the Solar course

# Remove students with just one post
h2o_ann_posts <- all_ann_posts %>% 
  filter(course == "Water") %>%
  select(-course) %>%
  filter(!student_id %in% h2o_one_post_stud)
length(unique(h2o_ann_posts$student_id))
# 679 students with at least 2 posts
# examine now the number of posts per stud
h2o_post_per_stud <- h2o_ann_posts %>% 
  group_by(student_id) %>%
  summarise(n_posts = n())
summary(h2o_post_per_stud$n_posts)
quantile(h2o_post_per_stud$n_posts, probs = seq(0.9,1,0.01))
# would have to remove students (i.e. sequences) w/ length above 99th percentile (68)
# UPDATE: better use 98th percentile (43) as the threshod
length(h2o_post_per_stud$student_id[h2o_post_per_stud$n_posts > 43])
# 14 students - remove them from further sequence analysis and examine their sequences manually?
h2o_outliers <- h2o_post_per_stud$student_id[h2o_post_per_stud$n_posts > 43]

# sort the posts, so that for each student, we have posts in the order 
# they appeared (based on the timestamp)
h2o_posts_sorted <- h2o_ann_posts %>% arrange(student_id, timestamp)
head(h2o_posts_sorted, n = 20)

# remove 'outliers'
h2o_posts_sorted <- h2o_posts_sorted %>%
  filter(!student_id %in% h2o_outliers)
# examine the effect of removal of outliers on the posts count
(nrow(h2o_ann_posts) - nrow(h2o_posts_sorted))/nrow(h2o_ann_posts)
# this led to the removal of 1135 posts, 21% of the original number


#############################################
# Create TraMineR squences for the 4 courses
#############################################
source("Sequencing_functions.R")
library(TraMineR)

###############
# Water course:
###############
h2o_seq_list <- create_course_seq(h2o_posts_sorted)
h2o_seq_df <- create_equal_length_seq(h2o_seq_list)
## create TraMineR sequences out of the sequences data frame
h2o_seq_final <- seqdef(data = h2o_seq_df, 
                        var = 2:ncol(h2o_seq_df), 
                        informat = "SPS", 
                        SPS.in = list(xfix = "", sdsep = "/"))
print(h2o_seq_final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence data, as well as the TraMineR sequence format
saveRDS(object = h2o_seq_df, file = "data/pre_processed/h2o_sequences_df.RData")
saveRDS(object = h2o_seq_final, file = "data/pre_processed/h2o_sequences_SPS_format.RData")


##############
# Solar course
##############
solar_seq_list <- create_course_seq(solar_posts_sorted)
solar_seq_df <- create_equal_length_seq(solar_seq_list)
## create TraMineR sequences out of the sequences data frame
solar_seq_final <- seqdef(data = solar_seq_df, 
                        var = 2:ncol(solar_seq_df), 
                        informat = "SPS", 
                        SPS.in = list(xfix = "", sdsep = "/"))
print(solar_seq_final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence data, as well as the TraMineR sequence format
saveRDS(object = solar_seq_df, file = "data/pre_processed/solar_sequences_df.RData")
saveRDS(object = solar_seq_final, file = "data/pre_processed/solar_sequences_SPS_format.RData")

## create also sequences for 'outliers', that is, sequences that stand apart for their length
solar_long_seq <- create_course_seq(solar_ann_posts 
                                    %>% filter(student_id %in% solar_outliers) %>%
                                      arrange(student_id, timestamp))
solar_long_df <- create_equal_length_seq(solar_long_seq)
solar_long_final <- seqdef(data = solar_long_df, 
                          var = 2:ncol(solar_long_df), 
                          informat = "SPS", 
                          SPS.in = list(xfix = "", sdsep = "/"))
print(solar_long_final[1:10, 1:30], format = 'SPS')
saveRDS(object = solar_long_df, file = "data/pre_processed/solar_long_seq_df.RData")
saveRDS(object = solar_long_final, file = "data/pre_processed/solar_long_seq_SPS_format.RData")

## To make the long sequences less granular and thus easier to interpret, 
## merge CT and CN states in C, and ST and SN into S states.
solar_long_sorted <- solar_ann_posts %>% 
  filter(student_id %in% solar_outliers) %>%
  arrange(student_id, timestamp) %>%
  mutate(code = as.character(code))
solar_long_sorted$code[solar_long_sorted$code %in% c('CT', 'CN')] <- "C"
solar_long_sorted$code[solar_long_sorted$code %in% c('ST', 'SN')] <- "S"
solar_lcompact_seq <- create_course_seq(solar_long_sorted)
solar_lcompact_df <- create_equal_length_seq(solar_lcompact_seq)
solar_lcompact_final <- seqdef(data = solar_lcompact_df, 
                           var = 2:ncol(solar_lcompact_df), 
                           informat = "SPS", 
                           SPS.in = list(xfix = "", sdsep = "/"))
print(solar_lcompact_final[1:10, 1:30], format = 'SPS')
saveRDS(object = solar_lcompact_df, file = "data/pre_processed/solar_long_compact_seq_df.RData")
saveRDS(object = solar_lcompact_final, file = "data/pre_processed/solar_long_compact_seq_SPS_format.RData")

###############
# Excel course:
###############
ex_seq_list <- create_course_seq(ex_posts_sorted)
ex_seq_df <- create_equal_length_seq(ex_seq_list)
## create TraMineR sequences out of the sequences data frame
ex_seq_final <- seqdef(data = ex_seq_df, 
                       var = 2:ncol(ex_seq_df), 
                       informat = "SPS", 
                       SPS.in = list(xfix = "", sdsep = "/"))
print(ex_seq_final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence data, as well as the TraMineR sequence format
saveRDS(object = ex_seq_df, file = "data/pre_processed/excel_sequences_df.RData")
saveRDS(object = ex_seq_final, file = "data/pre_processed/excel_sequences_SPS_format.RData")

############
# FP course
############
fp_seq_list <- create_course_seq(fp_posts_sorted)
fp_seq_df <- create_equal_length_seq(fp_seq_list)
## create TraMineR sequences out of the sequences data frame
fp_seq_final <- seqdef(data = fp_seq_df, 
                       var = 2:ncol(fp_seq_df), 
                       informat = "SPS", 
                       SPS.in = list(xfix = "", sdsep = "/"))
print(fp_seq_final[1:10, 1:30], format = 'SPS')

## store the data frame with sequence data, as well as the TraMineR sequence format
saveRDS(object = fp_seq_df, file = "data/pre_processed/FP_sequences_df.RData")
saveRDS(object = fp_seq_final, file = "data/pre_processed/FP_sequences_SPS_format.RData")
