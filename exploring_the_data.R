#install.packages('readxl')
library(readxl)
library(tidyverse)
library(quanteda)
library(udpipe)

#############################################
# Load the data: codes, Coh-metrix, LIWC
#############################################

# Load the annotated threads
# Note: even though the file name suggests that it is the posts that are annotated,
# that is not correct, the data entries represent discussion threads
# Note 2: these are not all discussion threads in the 4 examined courses, but 
# discussion threads with participation of the so called 'core posters'; 
# these were the subject of annotion
# Note 3: More precisely, to be annotated, a discussion thread had to:
# - have more than one person interacting, and 
# - at least one person in the thread needed to be a core poster, and 
# - at least one reply needed to take place within the same week as the original level 1 post in that thread
ann_posts <- read.csv("data/Annotated_Posts.csv", stringsAsFactors = FALSE)
str(ann_posts)
ann_posts <- ann_posts[,-1]

# The explanation of this (V2) variable is given in Sasha's thesis:
# p.154, Table 6.1 (overview); p.222, Appendix E (details)
# It is in fact, the code (label) assigned to the discussion threads 
# (of the core posters) in the (manual) coding process
unique(ann_posts$V2)

# Make the variable names more intuitive
colnames(ann_posts)[1:2] <- c("thread_id", "code")
ann_posts$code <- as.factor(ann_posts$code)
ann_posts$course <- as.factor(ann_posts$course)

summary(ann_posts)

# examine the codes more closely
table(ann_posts$code)
# accoding to Sasha, MIX and MIXED are the same; so, merge the two
ann_posts$code[ann_posts$code=='MIXED'] <- 'MIX'
# check if MIXED is removed
table(ann_posts$code)
# remove the MIXED category
ann_posts$code <- factor(ann_posts$code)

# examine the distribution of posts across courses and coding labels
table(ann_posts$course)
with(ann_posts, table(course, code))
with(ann_posts, round(prop.table(table(course, code), margin = 1), digits = 3))


# Load Coh-metrix computed for all discussion threads 
coh_metrix <- read.csv("data/CohMetrix.csv", stringsAsFactors = FALSE)
dim(coh_metrix)
# Note: the number of entries is far larger than in ann_posts, as these are Coh-metrix for all
# threads (and only threads of core posters were annotated) 

# create thread_id out of file name (TextID)
coh_metrix <- coh_metrix %>%
  mutate(thread_id = str_replace(string = TextID, pattern = ".+_content_only", replacement = "")) %>%
  mutate(thread_id = str_sub(thread_id, start = 2, end = -5)) %>%
  select(-TextID)
coh_metrix$thread_id[1:10]           

# Load Coh-metrix for the files that had some syntactical issues and could not be 
# processed directly (required some manual cleaning). 
# This file is complementary to "CohMetrix.csv"
coh_metrix_2 <- read.csv("data/cohmetrix-problemfiles.csv", stringsAsFactors = FALSE)
dim(coh_metrix_2)
coh_metrix_2 <- coh_metrix_2 %>%
  mutate(thread_id = str_replace(TextID, ".+original", "")) %>%
  mutate(thread_id = str_sub(thread_id, start = 2, end = -5)) %>%
  select(-TextID)
coh_metrix_2$thread_id          

coh_metrix <- rbind(coh_metrix, coh_metrix_2)
remove(coh_metrix_2)


# Load LIWC features computed for all discussion threads
liwc_data <- read_excel("data/LIWC2015_Results_all_texts.xlsx")
dim(liwc_data)

# create thread_id out of file name (Filename)
liwc_data <- liwc_data %>%
  mutate(thread_id = str_sub(Filename, end = -5)) %>%
  select(-Filename)
liwc_data$thread_id[1:10]


# check if the thread_id from LIWC and CohMetrix data frames are the same
setdiff(coh_metrix$thread_id, liwc_data$thread_id)
# 4 files w/ Coh_Metrix lack LIWC features
setdiff(liwc_data$thread_id, coh_metrix$thread_id)
# all LIWC annotated files have Coh_Metrix 
# check if these match the thread IDs in the ann_posts data frame
length(setdiff(ann_posts$thread_id, coh_metrix$thread_id))
# 339 posts in ann_posts do not have a corresponding post in coh_metrix
length(setdiff(ann_posts$thread_id, liwc_data$thread_id))
# again 339 posts in ann_posts do not have a corresponding post in liwc_data


# Create a data frame that will merge the code labels, coh-metrix and LIWC features
ann_threads <- merge(x = ann_posts, y = coh_metrix, by = "thread_id",
                     all.x = FALSE, all.y = FALSE)
ann_threads <- merge(x = ann_threads, y = liwc_data, by = "thread_id",
                     all.x = FALSE, all.y = FALSE)

# Save the thread annotations
saveRDS(ann_threads, "data/pre_processed/thread_annotations.RData")


#############################################
# Read in the text of the discussion threads
#############################################

# A function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  df <- data_frame(file = dir(infolder, full.names = TRUE)) %>%
        mutate(text_line = map(file, read_lines)) %>%
        transmute(id = basename(file), text_line) %>%
        unnest(text_line)  # text_line is a list-column; unnest transforms each element of the list into a row
  # Now, merge all pieces of text that belong to one file
  df %>%
    group_by(id) %>%
    summarise(text = str_c(text_line, collapse = " ")) %>%
    ungroup()
}

# Read the threads from the CTB (Water) course
ctb_content <- read_folder("data/content_files/ctb") %>%
  mutate(id = str_sub(id, end = -5))
# check how many ctb threads have annotations
length(intersect(ann_threads$thread_id, ctb_content$id))
# 1164
# keep only those threads that have annotations
ctb_content <- ctb_content %>%
  filter(id %in% ann_threads$thread_id) %>%
  mutate(course = "Water")

# Read the threads from the ET (Solar) course
et_content <- read_folder("data/content_files/et") %>%
  mutate(id = str_sub(id, end = -5))
# check how many et threads have annotations
length(intersect(ann_threads$thread_id, et_content$id))
# 1655
# keep only those threads that have annotations
et_content <- et_content %>%
  filter(id %in% ann_threads$thread_id)%>%
  mutate(course = "Solar")


# Read the threads from the ex (Excel) course
ex_content <- read_folder("data/content_files/ex") %>%
  mutate(id = str_sub(id, end = -5))
# check how many et threads have annotations
length(intersect(ann_threads$thread_id, ex_content$id))
# 1132
# keep only those threads that have annotations
ex_content <- ex_content %>%
  filter(id %in% ann_threads$thread_id)%>%
  mutate(course = "Excel")


# Read the threads from the fp (Functional programming) course
fp_content <- read_folder("data/content_files/fp") %>%
  mutate(id = str_sub(id, end = -5))
# check how many et threads have annotations
length(intersect(ann_threads$thread_id, fp_content$id))
# 889
# keep only those threads that have annotations
fp_content <- fp_content %>%
  filter(id %in% ann_threads$thread_id)%>%
  mutate(course = "FP")


# Merge discussion thread content of all four courses
all_content <- rbind(ctb_content, et_content, ex_content, fp_content)
all_content$course <- as.factor(all_content$course)

# Save the thread content df
saveRDS(all_content, "data/pre_processed/thread_content.RData")


###############################################################
# Examine non-curricular forum discourse in one of the courses.
# - focus on social-task (ST), social-non-task (SN), and mixed
#   (MIX) threads
# - use TextRank method to detect keywords and keyphrases
# - compare the keywords/keyphrases in forums of courses where
#   identity-formation processes did happen (Solar, FP) vs
#   forums where they were less present (Excel, Water)
###############################################################

with(ann_threads, table(code, course))

# select only 'social' threads
social_threads <- ann_threads %>%
  filter(code %in% c('ST', 'SN', 'MIX'))

# Focus on the social theads of the Solar course
# as an example of the course that, based on Sasha's analysis,
# has undergone the group formation process
solar_social_ids <- social_threads$thread_id[social_threads$course=="Solar"]
solar_social_content <- all_content %>%
  filter(id %in% solar_social_ids) %>%
  select(-course)

# examine the keywords / key phrases (udpipe + textrank)
# Load the appropriate language model (the one for English language)
tagger <- udpipe_load_model(file = "models/english-ud-2.0-170801.udpipe")
# Create morphological annotations of the thread text with the udpipe's built-in English tagger
# (this will produce POS tags, lemmas, ...)
solar_content_processed <- udpipe_annotate(tagger, solar_social_content$text)
solar_processed_df <- as.data.frame(solar_content_processed)
str(solar_processed_df)

# Examine a sample of lemmas and POS 
solar_processed_df[1:100, c(7,8)]

# check the number of unique nouns and adjectives
length(unique(solar_processed_df$lemma[solar_processed_df$upos %in% c("NOUN", "ADJ")]))
# 2555

library(textrank)
solar_social_keywords <- textrank::textrank_keywords(x = solar_processed_df$lemma,
                                            relevant = solar_processed_df$upos %in% c("NOUN", "ADJ"),
                                            p = 0.1, # keep 10% as relevant
                                            ngram_max = 3,
                                            sep = "_")
# Examine the top keywords, based on the PageRank
ssolar_keyw_pagerank <- sort(solar_social_keywords$pagerank$vector, decreasing = TRUE)
head(ssolar_keyw_pagerank, n = 50)
# Examine bigrams and trigrams
ssolar_keyphrases <- solar_social_keywords$keywords_by_ngram %>% filter(ngram > 1)
# Examine the top 20 bigrams
ssolar_keyphrases %>% filter(ngram == 2 ) %>%
  top_n(n=20)
# Top 20 ctb trigrams
ssolar_keyphrases %>% filter(ngram == 3) %>%
  top_n(n=20)


###############################################
# Examine posts data sets
# The datasets have the following structure: 
# student id, thread id, time stamp of posting
###############################################

# Examine Solar course
solar_posts <- read.csv("data/Solar_Posts.csv", stringsAsFactors = FALSE)
str(solar_posts)
solar_posts$X <- NULL
colnames(solar_posts) <- c('student_id', 'thread_id', 'timestamp')
# keep only annotated posts
solar_posts <- solar_posts %>%
  filter( thread_id %in% ann_threads$thread_id[ann_threads$course=="Solar"])
# check the number of students
length(unique(solar_posts$student_id))
# 1341
# check the number of threads per student
solar_thread_per_stud <- solar_posts %>%
  group_by(student_id) %>%
  summarise(n_thread = n())
summary(solar_thread_per_stud$n_thread)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.000   1.000   1.000   4.583   3.000  448.000
# plot the distribution
ggplot(solar_thread_per_stud,
       aes(x = n_thread)) + geom_histogram() + theme_bw()
# inspect this closer
table(solar_thread_per_stud$n_thread)
#   1   2   3
# 693 226 110

# Examine FP course
fp_posts <- read.csv("data/FP_Posts.csv", stringsAsFactors = FALSE)
str(fp_posts)
fp_posts$X <- NULL
colnames(fp_posts) <- c('student_id', 'thread_id', 'timestamp')
# keep only annotated posts
fp_posts <- fp_posts %>%
  filter( thread_id %in% ann_threads$thread_id[ann_threads$course=="FP"])
# check the number of students
length(unique(fp_posts$student_id))
# 884
# check the number of threads per student
fp_thread_per_stud <- fp_posts %>%
  group_by(student_id) %>%
  summarise(n_thread = n())
summary(fp_thread_per_stud$n_thread)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.000   1.000   2.000   7.388   5.000  805.000
# plot the distribution
ggplot(fp_thread_per_stud,
       aes(x = n_thread)) + geom_histogram() + theme_bw()
# inspect this closer
table(fp_thread_per_stud$n_thread)
#   1   2   3
# 371 164  63

# Examine Excel course
ex_posts <- read.csv("data/Excel_Posts.csv", stringsAsFactors = FALSE)
str(ex_posts)
ex_posts$X <- NULL
colnames(ex_posts) <- c('student_id', 'thread_id', 'timestamp')
# keep only annotated posts
ex_posts <- ex_posts %>%
  filter( thread_id %in% ann_threads$thread_id[ann_threads$course=="Excel"])
# check the number of students
length(unique(ex_posts$student_id))
# 1037
# check the number of threads per student
ex_thread_per_stud <- ex_posts %>%
  group_by(student_id) %>%
  summarise(n_thread = n())
summary(ex_thread_per_stud$n_thread)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.000   1.000   2.000   5.017   3.000    582.000 
# plot the distribution
ggplot(ex_thread_per_stud,
       aes(x = n_thread)) + geom_histogram() + theme_bw()
# inspect this closer
table(ex_thread_per_stud$n_thread)
#   1   2   3
# 518 174  87


# Examine Water course
h2o_posts <- read.csv("data/Water_Posts.csv", stringsAsFactors = FALSE)
str(h2o_posts)
h2o_posts$X <- NULL
colnames(h2o_posts) <- c('student_id', 'thread_id', 'timestamp')
# keep only annotated posts
h2o_posts <- h2o_posts %>%
  filter( thread_id %in% ann_threads$thread_id[ann_threads$course=="Water"])
# check the number of students
length(unique(h2o_posts$student_id))
# 1407
# check the number of unique threads
length(unique(h2o_posts$thread_id))
# 1164 
# check the number of threads per student
h2o_thread_per_stud <- h2o_posts %>%
  group_by(student_id) %>%
  summarise(n_thread = n())
summary(h2o_thread_per_stud$n_thread)
#  Min.  1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.000   1.000   1.000   4.299   3.000   211.000 
# plot the distribution
ggplot(h2o_thread_per_stud,
       aes(x = n_thread)) + geom_histogram() + theme_bw()
# inspect this closer
table(h2o_thread_per_stud$n_thread)
#   1   2   3
# 728 205 126
# check the number of student's posts per thread
h2o_posts_thread_and_stud <- h2o_posts %>%
  group_by(student_id, thread_id) %>%
  summarise(n_posts = n())
summary(h2o_posts_thread_and_stud$n_posts)
table(h2o_posts_thread_and_stud$n_posts)
# while large majority of threads received one post per student, there were also
# threads where some students posted multiple times

################################################
# Create a data set for sequence analysis:
# - associate eact student post with the code of
#   the thread the post belongs to
# - add a variable for the course
# - merge (annotated) posts from all the courses
################################################

solar_ann_posts <- merge(x = solar_posts, 
                         y = ann_posts %>% filter(course=="Solar"),
                         by = "thread_id", all.x = FALSE, all.y = FALSE)
fp_ann_posts <- merge(x = fp_posts, y = ann_posts %>% filter(course == "FP"),
                      by = "thread_id", all.x = FALSE, all.y = FALSE)
ex_ann_posts <- merge(x = ex_posts, y = ann_posts %>% filter(course == "Excel"),
                      by = "thread_id", all.x = FALSE, all.y = FALSE)
h2o_ann_posts <- merge(x = h2o_posts, y = ann_posts %>% filter(course == "Water"),
                       by = "thread_id", all.x = FALSE, all.y = FALSE)

all_ann_posts <- rbind(solar_ann_posts, fp_ann_posts, ex_ann_posts, h2o_ann_posts)
# transform timestamp into date object
all_ann_posts <- all_ann_posts %>% mutate(timestamp = as.POSIXct(timestamp, tz = "CET"))
# save the data
saveRDS(all_ann_posts, "data/pre_processed/all_annotated_posts.RData")
