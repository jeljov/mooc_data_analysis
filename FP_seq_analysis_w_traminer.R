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

# Clustering of sequences
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



#####################################################
# Examine long sequences from the FP course
# (seq. with length above 97th percentile)
#####################################################

