library(TraMineR)

# define color pallet
col_pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')

#####################################################
# Examining regular sequences from Solar course
#
# regular here means subset of sequences after outliers 
# (seq of length 1 and length above 97th percentile) 
# have been removed
#####################################################
solar_seq <- readRDS("data/pre_processed/solar_sequences_SPS_format.RData")
print(head(solar_seq, n=20), format = "SPS")

# check the length of the sequences
solar_seq_length <- as.vector(seqlength(solar_seq))
summary(solar_seq_length)
quantile(x = solar_seq_length, probs = seq(0.9, 1, 0.01))

# Index plot of all the sequences
# seqiplot(solar_seq, main = "Thread type sequences in the Solar course", 
#          border = NA, space=0, idxs=0, # this combination of the (3) parameters required for plotting all sequences
#          with.legend=F, ylab=NA, cex.axis=0.5)
# ## plot sequences up to medium length
# seq_med_len <- median(solar_seq_length)
# seq_up_to_median <- which(solar_seq_length <= seq_med_len)
# seqiplot(solar_seq, idxs = seq_up_to_median,
#          main = "Solar course: sequences of post types beneath the medium length", 
#          with.legend=F, ylab=NA, cex.axis=0.5, cpal = col_pallet, border = NA)
## Not useful at all!

# Examine state distribution by time points
par(mfrow=c(1,2))
seqdplot(solar_seq, main = "States (code) distribution through time", 
         cpal=col_pallet, with.legend=FALSE) #axes=FALSE, border=NA)
seqlegend(solar_seq, cex = 0.75, position = "top", cpal = col_pallet)
par(mfrow=c(1,1))

# Sequence frequency plot of the 20 most frequent sequences with bar width proportional
# to the frequencies
seqfplot(solar_seq, main = "Solar sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, cpal = col_pallet)
seqlegend(solar_seq, cpal = col_pallet)

# Sequence frequency plot of the 20 most frequent sequences with above median length
seqfplot(solar_seq[solar_seq_length > seq_med_len,], idxs = 1:20,
         main = "20 most frequent sequences with above median length", 
         with.legend=FALSE, axes=F, cpal = col_pallet)


# Examine directly the frequency of distinct sequences; 
# by default, seqtab f. will return 10 most frequent sequences;
# setting idxs=0 will result in a table of frequencies for all distinct sequences 
solar_seq_freq <- seqtab(solar_seq, idxs = 0) %>%
  attr(which = "freq") 
solar_seq_freq$seq <- unlist(attr(solar_seq_freq$Freq, "dimnames"))
solar_seq_freq <- solar_seq_freq %>% 
  arrange(desc(Freq)) %>%
  select(seq, Freq, Percent)
head(solar_seq_freq, n = 30)
tail(fp_seq_freq, n = 20)
# the frequent ones tend to be highly homogenious (only one thread type), 
# though sequences of 2 different thread types are not infrequent 


# Plot the mean number of post types per sequence
seqmtplot(solar_seq, main="Mean number of post types per sequence", 
          with.legend=FALSE, cpal = col_pallet, ylab = "Mean number of post types per sequence")

## Compute ENTROPY
## Entropy can be considered a measure of the diversity of states observed in a sequence
## It can also be considered a measure of 'uncertainty' of predicting the states in a given sequence
## Note: this entropy measure does not account for the ordering of the states in the sequence
solar_ent <- seqient(solar_seq)
# compare entropy values to sequences
tail(solar_ent)
print(tail(solar_seq), format = "SPS")
# plot the entropy
hist(solar_ent, xlab = "Entropy", main = NULL)

# Check the transition rates
round(seqtrate(solar_seq), digits = 2)

# Clustering of sequences
library(cluster)

# First, compute dissimilarities among sequences using a method that is 
# sensitive to distribution; according to Studer & Richardson (2016), 
# CHI-square and EUCLID distances with the number of periods (step param.) 
# equal to 1 would be a good selection
dist_euclid <- seqdist(seqdata = solar_seq, 
                        method = "EUCLID", 
                        norm = "auto",
                       step = 1)
solar_ward <- agnes(dist_euclid, diss = T, method = "ward")
plot(solar_ward)

# examine solution with 4 clusters
ward_cl4 <- cutree(solar_ward, k = 4)
table(ward_cl4)
# get the cluster assignment for each sequence
cl4_fac <- factor(ward_cl4, labels = paste("cluster:",1:4))

## plot the state distribution at each time point for each cluster
seqdplot(seqdata = solar_seq, 
         group = cl4_fac, 
         main = "Post type distribution over time",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(solar_seq, cpal = col_pallet, cex = 0.75)

# plot most frequent sequences from each cluster
seqfplot(solar_seq, group = cl4_fac,
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)


# Now, do the same, using a dissimilarity method that is 
# sensitive to timing; according to Studer & Richardson (2016), 
# suitable metrics would be CHI-square and EUCLID distances 
# with the number of periods ('step' param.) equal to the length 
# of sequences 
dist_euclid_2 <- seqdist(seqdata = solar_seq,
                     method = "EUCLID",
                     norm = "auto",
                     step = max(solar_seq_length))
solar_euclid_2_ward <- agnes(dist_euclid_2, diss = TRUE, method = "ward")
plot(solar_euclid_2_ward)
# does not look promising...
# choose 4 clusters
ward_2_cl4 <- cutree(solar_euclid_2_ward, k = 4)
table(ward_2_cl4)
# get the cluster assignment for each sequence
cl4_2_fac <- factor(ward_2_cl4, labels = paste("cluster:",1:4))
seqdplot(seqdata = solar_seq, 
         group = cl4_2_fac, 
         main = "Post type distribution over time",
         with.legend=FALSE, cpal = col_pallet)

seqfplot(solar_seq, group = cl4_2_fac,
         main = "Sequence frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)



# Now, use a set of features to decribe thread types, and based on these features
# compute state-substitution costs; the computed costs are then used as an input
# for the Optimal Matching method for estimating sequence dissimilarity; 
# the computed dissimilarities are then used for clustering 
source("Sequencing_functions.R")
subs_cost <- features_based_subs_cost(solar_seq)
dist_om <- seqdist(seqdata = solar_seq, 
                   method = "OM", 
                   sm = subs_cost$sm, indel = 0.75,
                   norm = "auto")
solar_OM_ward <- agnes(dist_om, diss = TRUE, method = "ward")
plot(solar_OM_ward)
# examine 4, 5 and 6 clusters
ward_3_cl4 <- cutree(solar_OM_ward, k = 4)
ward_3_cl5 <- cutree(solar_OM_ward, k = 5)
ward_3_cl6 <- cutree(solar_OM_ward, k = 6)
table(ward_3_cl6)
# get the cluster assignment for each sequence
seqdplot(seqdata = solar_seq, 
         group = factor(ward_3_cl4), 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
seqlegend(solar_seq, cpal = col_pallet)

seqfplot(solar_seq, group = factor(ward_3_cl4),
         main = "Seq. freq. plot", 
         with.legend=FALSE, axes=F, idxs=1:20, 
         cpal = col_pallet)
# At the end, the solution with 4 clusters seemed to be the best one 

#####################################################
# Examine long sequences from the Solar course
# (seq. with length above 97th percentile)
#####################################################

solar_long_seq <- readRDS("data/pre_processed/solar_long_seq_SPS_format.RData")
print(solar_long_seq, format = "SPS")

solar_long_len <- seqlength(solar_long_seq)
summary(solar_long_len)
quantile(solar_long_len, probs = seq(0.7,1,0.05))

sorted_long_seq <- solar_long_seq[order(solar_long_len),]
seqiplot(sorted_long_seq, main = "Solar: overly long sequences (N=18)", 
         with.legend=FALSE, yaxis = FALSE, axes = FALSE, 
         idxs=0, cpal = col_pallet, border = NA)

seqfplot(solar_long_seq, main = "Solar overly long sequences - frequency plot", 
         with.legend=FALSE, axes=F, idxs=1:10, cpal = col_pallet, border = NA)
# these sequences are so long that each one is unique, no grouping is possible

seqdplot(solar_long_seq, main = "States (code) distribution through time", cpal=col_pallet, 
         with.legend=FALSE, border=NA)
seqlegend(solar_long_seq, cex = 0.75, cpal = col_pallet)

# Check the transition rates
round(seqtrate(solar_long_seq), digits = 2)

# Examine entropy
hist(seqient(solar_long_seq), xlab = "Entropy", main = NULL)
# tends to be very high - as expected