library(TraMineR)
library(tidyverse)

# define color pallet
col_pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')

#####################################################
# Examining regular sequences from the Water course
#
# regular here means subset of sequences after outliers 
# (seq of length 1 and length above 98th percentile) 
# have been removed
#####################################################
h2o_seq <- readRDS("data/pre_processed/h2o_sequences_SPS_format.RData")
print(head(h2o_seq, n=20), format = "SPS")

# check the length of the sequences
h2o_seq_length <- as.vector(seqlength(h2o_seq))
summary(h2o_seq_length)
hist(h2o_seq_length, main = NULL, xlab = "Sequence length")
quantile(x = h2o_seq_length, probs = seq(0.9, 1, 0.01))

# Examine state distribution by time points
seqdplot(h2o_seq, main = "Distribution of communication intents over the Water course", 
         cpal=col_pallet, with.legend=FALSE) #axes=FALSE, border=NA)
seqlegend(h2o_seq, cex = 0.75, cpal = col_pallet)

# Sequence frequency plot of the 20 most frequent sequences with bar width proportional
# to the frequencies
seqfplot(h2o_seq, main = "Water course: 20 most freq. sequences", 
         with.legend=FALSE, axes=F, idxs=1:20, cpal = col_pallet)
seqlegend(h2o_seq, cpal = col_pallet)

# Sequence frequency plot of the 20 most frequent sequences with above median length
seq_med_len <- median(h2o_seq_length)
seqfplot(h2o_seq[h2o_seq_length > seq_med_len,], idxs = 1:20,
         main = "Water course: 20 most freq. seq. w/ above median length", 
         with.legend=FALSE, axes=F, cpal = col_pallet)

# Examine directly the frequency of distinct sequences; 
# by default, seqtab f. will return 10 most frequent sequences;
# setting idxs=0 will result in a table of frequencies for all distinct sequences 
h2o_seq_freq <- seqtab(h2o_seq, idxs = 0) %>%
  attr(which = "freq") 
h2o_seq_freq$seq <- unlist(attr(h2o_seq_freq$Freq, "dimnames"))
h2o_seq_freq <- h2o_seq_freq %>% 
  arrange(desc(Freq)) %>%
  select(seq, Freq, Percent)
head(h2o_seq_freq, n = 20)
tail(fp_seq_freq, n = 20)
# the frequent ones are rather homogenious - almost all top20 sequences have at most
# two different thread types, and half of them are one-thread-type sequences

# Plot the mean number of communication intent types per sequence
seqmtplot(h2o_seq, main="Mean number of communication intent types per sequence", 
          with.legend=FALSE, cpal = col_pallet, 
          ylab = "")

## Compute ENTROPY
## Note: this entropy measure does not account for the ordering of the states in the sequence
h2o_ent <- seqient(h2o_seq)
# compare entropy values to sequences
h2o_first_20 <- as.character(print(head(h2o_seq, n=20), format = "SPS"))
cbind(Seq = h2o_first_20, Entropy = round(h2o_ent[1:20,1], digits = 3))
# plot the entropy
hist(h2o_ent, xlab = "Entropy", main = NULL)

# Check the transition rates
round(seqtrate(h2o_seq), digits = 2)

# Clustering of sequences
library(cluster)

# First, compute dissimilarities among sequences using a method that is 
# sensitive to distribution; according to Studer & Richardson (2016), 
# CHI-square and EUCLID distances with the number of periods (step param.) 
# equal to 1 would be a good selection
dist_euclid <- seqdist(seqdata = h2o_seq, 
                        method = "EUCLID", 
                        norm = "auto",
                       step = 1)
h2o_euclid_1_ward <- agnes(dist_euclid, diss = T, method = "ward")
plot(h2o_euclid_1_ward)

# examine solutions with 4 and 5 clusters
ward_cl5 <- cutree(h2o_euclid_1_ward, k = 5)
table(ward_cl5)
ward_cl4 <- cutree(h2o_euclid_1_ward, k = 4)
table(ward_cl4)
# get the cluster assignment for each sequence
cl5_fac <- factor(ward_cl5, labels = paste("clust:",1:5))
cl4_fac <- factor(ward_cl4, labels = paste("clust:",1:4))

## plot the state distribution at each time point for each cluster
seqdplot(seqdata = h2o_seq, 
         group = cl5_fac, 
         main = "Distribution of thread types over the course",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(h2o_seq, cpal = col_pallet, cex = 0.75)

# plot most frequent sequences from each cluster
seqfplot(h2o_seq, group = cl5_fac,
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)

# Take a closer look at the frequency plot of each cluster in turn
cl <- 5
seqfplot(h2o_seq[ward_cl5 == cl,],
         main = paste("Freq. plot for cluster",cl), 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)
seqtab(h2o_seq[ward_cl5 == cl,], idxs = 1:20)
hist(seqient(h2o_seq[ward_cl5 == cl,]), xlab = "Entropy", main = NULL)


# Compute the level of discrepancy for each seq. cluster
# Discrepancy is defined as an extension of the concept of variance 
# to any kind of objects for which pairwise dissimilarities can be computed 
ward_cl5_var <- vector()
for(clust in 1:5) {
  clust_dist <- seqdist(seqdata = h2o_seq[ward_cl5 == clust,],
                        method = "EUCLID", step = 1, norm = "auto")
  ward_cl5_var[clust] <- dissvar(clust_dist)
}
round(ward_cl5_var, digits = 2)
# 1.95 1.13 1.19 0.85 2.43
# The results suggest high variability among sequences in the 1st and 5th cluster;
# the other 3 clusters are  more or less fine, that is, not much dispersion is present.
# This is expected as 1st and 5th clusters are clusters with longer sequences.


# Plot the representative sequences of each cluster

## notes regarding the representative sequence (rseq) plot:
# - width of a rseq is proportional to the number of sequences assigned to that rseq
# - sequences are plotted bottom-up according to their representativeness score
# - coverage is the percentage of sequences assigned to one of the rseq (ie. sequences that are 
#   in the neighbourhood of the rseq, where the border of the neighbourhood is determined by the
#   'pradius' parameter); min coverage is specified via the 'coverage' parameter  
# - the axis A of the horizontal line above the plot represents for each rseq the (pseudo) 
#   variance within the subset of sequences assigned to that rseq 
# - the axis B represents the mean distance between each rseq and sequences assigned to that rseq
# - the horizontal line (related to the A and B axes) ranges from 0 to the 
#   max. theoretical distance between two sequences (in the metric used for 
#   computing dissimilarity between sequences)

for(clust in 1:5) {
  clust_dist <- seqdist(seqdata = h2o_seq[ward_cl5 == clust,],
                        method = "EUCLID", step = 1, norm = "auto")
  seqrplot(seqdata = h2o_seq[ward_cl5 == clust,], 
           diss=clust_dist, 
           criterion="prob", pradius=0.25, coverage=0.5,
           main = paste("Representative seq. for cluster", clust), 
           with.legend=FALSE, cpal = col_pallet)
}



# Now, do the same, using a dissimilarity method that is 
# sensitive to timing; according to Studer & Richardson (2016), 
# suitable metrics would be CHI-square and EUCLID distances 
# with the number of periods ('step' param.) equal to the length 
# of sequences 
dist_euclid_2 <- seqdist(seqdata = h2o_seq,
                     method = "EUCLID",
                     norm = "auto",
                     step = maedian(h2o_seq_length))
h2o_euclid_2_ward <- agnes(dist_euclid_2, diss = TRUE, method = "ward")
plot(h2o_euclid_2_ward)
# Tried with both with max and median length, but both produced poor clustering results


# Now, use a set of features to decribe thread types, and based on these features
# compute state-substitution costs; the computed costs are then used as an input
# for the Optimal Matching method for estimating sequence dissimilarity; 
# the computed dissimilarities are then used for clustering 
source("Sequencing_functions.R")
subs_cost <- features_based_subs_cost(h2o_seq)
dist_om <- seqdist(seqdata = h2o_seq, 
                   method = "OM", sm = subs_cost$sm, indel = 0.75,
                   norm = "auto")
h2o_OM_ward <- agnes(dist_om, diss = TRUE, method = "ward")
plot(h2o_OM_ward)
# choose 5 clusters
ward_3_cl5 <- cutree(h2o_OM_ward, k = 5)
table(ward_3_cl5)
# get the cluster assignment for each sequence
cl5_3_fac <- factor(ward_3_cl5, labels = paste("clust:",1:5))
seqdplot(seqdata = h2o_seq, 
         group = cl5_3_fac, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(h2o_seq, cpal = col_pallet)

seqfplot(h2o_seq, group = cl5_3_fac,
         main = "Seq. freq. plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)

# Take a closer look at the frequency plot of each cluster in turn
cl <- 5
seqfplot(h2o_seq[ward_3_cl5 == cl,],
         main = paste("Freq. plot for cluster",cl), 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)


# Compute the level of discrepancy for each seq. cluster
ward_3_cl5_var <- vector()
for(clust in 1:5) {
  clust_dist <- seqdist(seqdata = h2o_seq[ward_3_cl5 == clust,],
                     method = "OM", sm = subs_cost$sm, indel = 0.75)
  ward_3_cl5_var[clust] <- dissvar(clust_dist)
}
round(ward_3_cl5_var, digits = 2)
# 5.40 1.47 1.14 0.37 1.64
# The results suggest high variability among sequences in the 1st cluster;
# the other 4 clusters are fine, that is, not much dispersion is present.
# This is expected as the 1st cluster stands apart for its longer sequences.


# Plot the representative sequences of each cluster
for(clust in 1:5) {
  clust_dist <- seqdist(seqdata = h2o_seq[ward_3_cl5 == clust,],
                        method = "OM", sm = subs_cost$sm, indel = 0.75)
  seqrplot(seqdata = h2o_seq[ward_3_cl5 == clust,], 
           diss=clust_dist, 
           criterion="density", pradius=0.25, coverage=0.75,
           main = paste("Representative seq. for cluster", clust), 
           with.legend=FALSE, cpal = col_pallet)
}



