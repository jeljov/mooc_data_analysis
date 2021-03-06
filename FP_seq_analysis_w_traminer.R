library(TraMineR)
library(tidyverse)

# define color pallet
col_pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')

#####################################################
# Examining regular sequences from the FP course
#
# regular here means subset of sequences after outliers 
# (seq of length 1 and length above 97th percentile) 
# have been removed
#####################################################
fp_seq <- readRDS("data/pre_processed/FP_sequences_SPS_format.RData")
print(head(fp_seq, n=20), format = "SPS")

# check the length of the sequences
fp_seq_length <- as.vector(seqlength(fp_seq))
summary(fp_seq_length)
hist(fp_seq_length, main = NULL, xlab = "Sequence length")
quantile(x = fp_seq_length, probs = seq(0.9, 1, 0.01))

# Examine state distribution by time points
seqdplot(fp_seq, main = "Distribution of communication intents over the FP course", 
         cpal=col_pallet, with.legend=FALSE) #axes=FALSE, border=NA)
seqlegend(fp_seq, cex = 0.75, cpal = col_pallet)

# Sequence frequency plot of the 20 most frequent sequences with bar width proportional
# to the frequencies
seqfplot(fp_seq, main = "FP course: 20 most freq. sequences", 
         with.legend=FALSE, axes=F, idxs=1:20, cpal = col_pallet)
seqlegend(fp_seq, cpal = col_pallet)

# Sequence frequency plot of the 20 most frequent sequences with above median length
seq_med_len <- median(fp_seq_length)
seqfplot(fp_seq[fp_seq_length > seq_med_len,], idxs = 1:20,
         main = "FP course: 20 most freq. seq. w/ above median length", 
         with.legend=FALSE, axes=F, cpal = col_pallet)

# Examine directly the frequency of distinct sequences; 
# by default, seqtab f. will return 10 most frequent sequences;
# setting idxs=0 will result in a table of frequencies for all distinct sequences 
fp_seq_freq <- seqtab(fp_seq, idxs = 0) %>%
  attr(which = "freq") 
fp_seq_freq$seq <- unlist(attr(fp_seq_freq$Freq, "dimnames"))
fp_seq_freq <- fp_seq_freq %>% 
  arrange(desc(Freq)) %>%
  select(seq, Freq, Percent)
head(fp_seq_freq, n = 20)
tail(fp_seq_freq, n = 20)
# the frequent ones are rather homogenious; one-thread-type sequences are far more present, 
# among the top frequent seq, than it is the case in the Solar course

# Plot the mean number of communication intent types per sequence
seqmtplot(fp_seq, main="Mean number of communication intent types per sequence", 
          with.legend=FALSE, cpal = col_pallet, 
          ylab = "")

## Compute ENTROPY
## Entropy can be considered a measure of the diversity of states observed in a sequence
## It can also be considered a measure of 'uncertainty' of predicting the states in a given sequence
## Note: this entropy measure does not account for the ordering of the states in the sequence
fp_ent <- seqient(fp_seq)
# compare entropy values to sequences
fp_first_20 <- as.character(print(head(fp_seq, n=20), format = "SPS"))
cbind(Seq = fp_first_20, Entropy = round(fp_ent[1:20,1], digits = 3))
# plot the entropy
hist(fp_ent, xlab = "Entropy", main = NULL)

# Check the transition rates
round(seqtrate(fp_seq), digits = 2)

#########################
# Clustering of sequences
#########################
library(cluster)

# First, compute dissimilarities among sequences using a method that is 
# sensitive to distribution; according to Studer & Richardson (2016), 
# CHI-square and EUCLID distances with the number of periods (step param.) 
# equal to 1 would be a good selection
dist_euclid <- seqdist(seqdata = fp_seq, 
                        method = "EUCLID", 
                        norm = "auto",
                       step = 1)
fp_euclid_1_ward <- agnes(dist_euclid, diss = T, method = "ward")
plot(fp_euclid_1_ward)

# examine solution with 5 clusters
ward_cl5 <- cutree(fp_euclid_1_ward, k = 5)
table(ward_cl5)
# get the cluster assignment for each sequence
cl5_fac <- factor(ward_cl5, labels = paste("clust:",1:5))

## plot the state distribution at each time point for each cluster
seqdplot(seqdata = fp_seq, 
         group = cl5_fac, 
         main = "Distribution of thread types over the course",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(fp_seq, cpal = col_pallet, cex = 0.75)

# plot most frequent sequences from each cluster
seqfplot(fp_seq, group = cl5_fac,
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)

# Take a closer look at the 3rd cluster - it looks rather noisy
seqfplot(fp_seq[ward_cl5 == 3,],
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)
seqtab(fp_seq[ward_cl5 == 3,], idxs = 1:20)
hist(seqient(fp_seq[ward_cl5 == 3,]), xlab = "Entropy", main = NULL)

# associate the clusters with student ids and save them
# for later processing
fp_seq_df <- readRDS("data/pre_processed/fp_sequences_df.RData")
fp_ward_cl5 <- data.frame(student_id = fp_seq_df$student_id,
                          cl5 = ward_cl5)
saveRDS(fp_ward_cl5, "clustering/fp_ward_euclid_5_clust.RData")



# Now, do the same, using a dissimilarity method that is 
# sensitive to timing; according to Studer & Richardson (2016), 
# suitable metrics would be CHI-square and EUCLID distances 
# with the number of periods ('step' param.) equal to the length 
# of sequences 
# UPDATE: used median, as length differs a lot across sequences
dist_euclid_2 <- seqdist(seqdata = fp_seq,
                     method = "EUCLID",
                     norm = "auto",
                     step = median(fp_seq_length))
fp_euclid_2_ward <- agnes(dist_euclid_2, diss = TRUE, method = "ward")
plot(fp_euclid_2_ward)
# choose 5 clusters
ward_2_cl5 <- cutree(fp_euclid_2_ward, k = 5)
table(ward_2_cl5)
# get the cluster assignment for each sequence
cl5_2_fac <- factor(ward_2_cl5, labels = paste("cluster:",1:5))
seqdplot(seqdata = fp_seq, 
         group = cl5_2_fac, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(fp_seq, cpal = col_pallet)

seqfplot(fp_seq, group = cl5_2_fac,
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)

## Not much difference between this and the previous clustering solution 


# Now, use a set of features to decribe thread types, and based on these features
# compute state-substitution costs; the computed costs are then used as an input
# for the Optimal Matching method for estimating sequence dissimilarity; 
# the computed dissimilarities are then used for clustering 
source("Sequencing_functions.R")
subs_cost <- features_based_subs_cost(fp_seq)
dist_om <- seqdist(seqdata = fp_seq, 
                   method = "OM", sm = subs_cost$sm, indel = 0.75,
                   norm = "auto")
fp_OM_ward <- agnes(dist_om, diss = TRUE, method = "ward")
plot(fp_OM_ward)
# choose 5 clusters
ward_3_cl5 <- cutree(fp_OM_ward, k = 5)
table(ward_3_cl5)
# get the cluster assignment for each sequence
cl5_3_fac <- factor(ward_3_cl5, labels = paste("clust:",1:5))
seqdplot(seqdata = fp_seq, 
         group = cl5_3_fac, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(fp_seq, cpal = col_pallet)

seqfplot(fp_seq, group = cl5_3_fac,
         main = "Seq. freq. plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)


# Associate the cluster assignments with student ids and save them
# for later processing
fp_ward_om_cl5 <- data.frame(student_id = fp_seq_df$student_id,
                             cl5 = ward_3_cl5)
saveRDS(fp_ward_om_cl5, "clustering/fp_ward_OM_5_clust.RData")


####################################################################
# Compare clusters w.r.t. the features indicative of 
# the level of student engagement in the discussion forums
# - engaged_count - the number of weeks when the learner posted to 
#   the discussion forum
# - engaged_scope - the number of weeks between the very first week 
#   the learner posted and the week of the last post
# - core_poster - those with engaged_count >= 3
####################################################################
source("util_functions.R")
library(knitr)

# load the features about students' forum engagement
forum_features <- read.csv("data/lca_features/lca_groups_fp.csv")
str(forum_features)
# remove the variables not required for analysis
forum_features <- forum_features[,-1]
# rename variables to better reflect their meaning
colnames(forum_features) <- c("student_id", "engaged_count", "engaged_scope")
# add the core_poster feature
forum_features$core_poster <- FALSE
forum_features$core_poster[forum_features$engaged_count >= 3] <- TRUE


#######################################################################
# Comparison of the clusters obtained by applying the Ward's algorithm 
# on sequence similarities computed using the metric sensitive to 
# state distribution (Euclid distance with step=1)
#######################################################################

# Read in cluster assignments
euclid_clusters <- readRDS("clustering/fp_ward_euclid_5_clust.RData") 
# Merge the cluster assignments w/ forum features data, keeping only
# the data for students with hand-coded threads
euclid_clust_data <- merge(x = euclid_clusters, y = forum_features,
                           by = "student_id", all.x = TRUE, all.y = FALSE)
which(!complete.cases(euclid_clust_data))
# features available for all the students

# examine the data
table(euclid_clust_data$engaged_count)
round(prop.table(table(euclid_clust_data$engaged_count)), digits = 2)
table(euclid_clust_data$engaged_scope)
round(prop.table(table(euclid_clust_data$engaged_scope)), digits = 2)
table(euclid_clust_data$core_poster)
round(prop.table(table(euclid_clust_data$core_poster)), digits = 2)

# Compare clusters w.r.t. the forum engagement features
euclid_clust_stats <- summary.stats(euclid_clust_data[,c(3,4)], euclid_clust_data$cl5)
kable(euclid_clust_stats, format = 'rst')
# check the distribution of core posters
with(euclid_clust_data, table(core_poster, cl5))
with(euclid_clust_data, round(prop.table(table(core_poster, cl5), margin = 2),digits = 2))

# Use statistical tests to compare clusters based on the given features
# Check first if the features are normally distributed
shapiro.test(euclid_clust_data$engaged_count)
hist(euclid_clust_data$engaged_count)
shapiro.test(euclid_clust_data$engaged_scope)
hist(euclid_clust_data$engaged_scope)
# no, not even nearly => use non-parametric tests

kruskal.test(engaged_count ~ cl5, data = euclid_clust_data)
# Kruskal-Wallis chi-squared = 124.18, df = 4, p-value < 2.2e-16
kruskal.test(engaged_scope ~ cl5, data = euclid_clust_data)
# Kruskal-Wallis chi-squared = 81.118, df = 4, p-value < 2.2e-16
# =>
# Singificant difference is present among the clusters for both examined features
# Use pairwise tests to examine where exactly (i.e. between which cluster pairs) 
# the difference is present

# First, do pairwise comparisons for the engaged_count feature
engage_cnt_comparison <- pairwise.compare.Mann.Whitney(euclid_clust_data, 'engaged_count', 'cl5', 5) 
kable(x = engage_cnt_comparison, format = 'rst')

# Now, do pairwise comparisons for the engaged_scope feature
engage_scope_comparison <- pairwise.compare.Mann.Whitney(euclid_clust_data, 'engaged_scope', 'cl5', 5) 
kable(x = engage_scope_comparison, format = 'rst')


#######################################################################
# Comparison of the clusters obtained by applying the Ward's algorithm 
# on sequence similarities computed using the OM metric 
#######################################################################

# Read in cluster assignments
om_clusters <- readRDS("clustering/fp_ward_OM_5_clust.RData") 
# Merge the cluster assignments w/ forum features data, keeping only
# the data for students with hand-coded threads
om_clust_data <- merge(x = om_clusters, y = forum_features,
                       by = "student_id", all.x = TRUE, all.y = FALSE)
which(!complete.cases(om_clust_data))
# no missing feature data

# Compare clusters w.r.t. the forum engagement features
om_clust_stats <- summary.stats(om_clust_data[,c(3,4)], om_clust_data$cl5)
kable(om_clust_stats, format = 'rst')
# check the distribution of core posters
with(om_clust_data, table(core_poster, cl5))
with(om_clust_data, round(prop.table(table(core_poster, cl5), margin = 2),digits = 2))

# Use statistical tests to compare clusters based on the given features
kruskal.test(engaged_count ~ cl5, data = om_clust_data)
# Kruskal-Wallis chi-squared = 149.2, df = 4, p-value < 2.2e-16
kruskal.test(engaged_scope ~ cl5, data = om_clust_data)
# Kruskal-Wallis chi-squared = 97.438, df = 4, p-value < 2.2e-16
# =>
# Singificant difference is present among the clusters for both examined features
# Use pairwise tests to examine where exactly (i.e. between which cluster pairs) 
# the difference is present

# First, do pairwise comparisons for the engaged_count feature
engage_cnt_comparison <- pairwise.compare.Mann.Whitney(om_clust_data, 'engaged_count', 'cl5', 5) 
kable(x = engage_cnt_comparison, format = 'rst')

# Now, do pairwise comparisons for the engaged_scope feature
engage_scope_comparison <- pairwise.compare.Mann.Whitney(om_clust_data, 'engaged_scope', 'cl5', 5) 
kable(x = engage_scope_comparison, format = 'rst')
