#install.packages("apcluster")
library(apcluster)
library(TraMineR)

# load the data
h2o_seq <- readRDS("data/pre_processed/h2o_sequences_SPS_format.RData")
h2o_df <- readRDS("data/pre_processed/h2o_sequences_df.RData")

# define color pallet
col_pallet <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')

##########################################################################
# First, use a distance method that is sensitive to distribution of states
# In particular, Eucledian distance with step = 1 
##########################################################################
dist_euclid <- seqdist(seqdata = h2o_seq, 
                       method = "EUCLID", 
                       norm = "auto",
                       step = 1)
# AP requires similarity matrix as its input
# ("the higher the value, the more similar two samples")
# So, distances need to be transformed into similarities
# This can be done in different ways:
# a) sim = 1/(1+dist) - tried first, gave poor results 
sim_euclid_1 <- 1/(1+dist_euclid)
# set student ids as column names of the similarity matrix 
# (required for later identification of exemplars)
colnames(sim_euclid_1) <- h2o_df$student_id
# b) sim = 1 - dist/max(dist) - produced better results than a)
max_dist <- max(dist_euclid)
sim_euclid_2 <- 1 - (dist_euclid/max_dist)
colnames(sim_euclid_2) <- h2o_df$student_id
# c) compute similarities as negative squared distances 
# as it is done by the negDistMat() f. - still poor results
sim_euclid_3 <- -(t(apply(X = dist_euclid, MARGIN = 1, FUN = function(x) x^2)))
colnames(sim_euclid_3) <- h2o_df$student_id
# Run AP algorithm
set.seed(310318)
apclust_euclid <- apcluster(s = sim_euclid_3, 
                            details = TRUE, 
                            q = 0)
length(apclust_euclid@exemplars)
# large number of clusters, even with the preferences set in such a way that 
# minimum number of clusters is generated
# Should apply agglomerative clustering on top of these results to 
# obtain more meaningful (interpretable) and useful results
# Before that, visualise the examplars to get an idea of what they look like
# Use frequency plot for that
seqfplot(h2o_seq[apclust_euclid@exemplars,], 
         main = "Water course: AP (with Eucledian similarity) exemplars", 
         with.legend=FALSE, axes=F, 
         idxs=0, 
         cpal = col_pallet)
# Now, use agglomerative clustering to merge clusters obtained through AP
agg_ap_euclid <- aggExCluster(x = apclust_euclid, s = sim_euclid_3)
plot(agg_ap_euclid)
# cut tree so that 4 clusters are obtained 
agg_ap_euclid_4cl <- cutree(agg_ap_euclid, k = 4)
# number of sequences per cluster
table(agg_ap_euclid_4cl@idx)
# very uneven...
cl4 <- factor(as.integer(agg_ap_euclid_4cl@idx), 
              levels = agg_ap_euclid_4cl@exemplars,
              labels = paste0("cl_", 1:4))
seqdplot(seqdata = h2o_seq, 
         group = cl4, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
# no, not useful...

# Use Exemplar-based Agglomerative Clustering directly?
agg_euclid <- aggExCluster(s = sim_euclid_3)
plot(agg_euclid)
# OMG! Not useful at all...

# Force the number of clusters 
# Set 5 as the number of clusters, since that was the number that proved the best in
# agglomerative clustering with the Ward's algoritm
ap_euclid_5cl <- apclusterK(s = sim_euclid_3, K = 5, details = TRUE)
# examine exemplars
seqfplot(h2o_seq[ap_euclid_5cl@exemplars,], 
         main = "Water course: AP (with Eucledian similarity) exemplars", 
         with.legend=FALSE, axes=F, 
         idxs=0, 
         cpal = col_pallet)
# examine the 5 clusters
table(ap_euclid_5cl@idx)
# not bad in terms of eveness of counts 
cl5 <- factor(as.integer(ap_euclid_5cl@idx), 
              levels = ap_euclid_5cl@exemplars,
              labels = paste0("cl_", 1:5))
seqdplot(seqdata = h2o_seq, 
         group = cl5, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
# does not look promising...


#####################################################################
# Now, apply the same procedure but using the Optimal Matching method
# with feature-based state-substitution costs
#####################################################################
source("Sequencing_functions.R")
subs_cost <- features_based_subs_cost(h2o_seq)
dist_om <- seqdist(seqdata = h2o_seq, 
                   method = "OM", sm = subs_cost$sm, 
                   indel = 0.75,
                   norm = "auto")
# Transform distances into similarities
# a) sim = 1/(1+dist) 
sim_om_1 <- 1/(1+dist_om)
colnames(sim_om_1) <- h2o_df$student_id
# b) sim = 1 - dist/max(dist) 
dist_om_max <- max(dist_om)
sim_om_2 <- 1-(dist_om/dist_om_max)
colnames(sim_om_2) <- h2o_df$student_id
# c) compute similarities as negative squared distances 
# as it is done by the negDistMat() f.
sim_om_3 <- -(t(apply(X = dist_om, MARGIN = 1, FUN = function(x) x^2)))
colnames(sim_om_3) <- h2o_df$student_id
# Run AP algorithm
set.seed(310318)
apclust_om <- apcluster(s = sim_om_3,
                            details = TRUE, 
                            q = 0)
length(apclust_om@exemplars)
# high number of clusters even with the preferences set to minimal number of clusters
# Visualise the examplars to get an idea of what they look like
# Use frequency plot for that
seqfplot(h2o_seq[apclust_om@exemplars,], 
         main = "Water course: AP (with OM similarity) exemplars", 
         with.legend=FALSE, axes=F, 
         idxs=0, 
         cpal = col_pallet)
# Now, use agglomerative clustering to merge clusters obtained through AP
agg_ap_om <- aggExCluster(x = apclust_om, s = sim_om_3)
plot(agg_ap_om)
agg_ap_om_6cl <- cutree(agg_ap_om, k = 6)
agg_ap_om_6cl@exemplars
# number of sequences per cluster
table(agg_ap_om_6cl@idx)
# very uneven...
cl6 <- factor(as.integer(agg_ap_om_6cl@idx), 
              levels = agg_ap_om_6cl@exemplars,
              labels = paste0("cl_", 1:6))
seqdplot(seqdata = h2o_seq, 
         group = cl6, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
# does not look very promising...

# Force the number of clusters 
set.seed(310318)
ap_om_5cl <- apclusterK(s = sim_om_3, K = 5, details = TRUE,
                        convits = 200, maxits = 2000)
plot(ap_om_5cl)
# examine exemplars
seqfplot(h2o_seq[ap_om_5cl@exemplars,], 
         main = "Water course: AP (with OM similarity) exemplars", 
         with.legend=FALSE, axes=F, 
         idxs=0, 
         cpal = col_pallet)
# examine the 5 clusters
table(ap_om_5cl@idx)
# not bad in terms of eveness of counts 
cl5 <- factor(as.integer(ap_om_5cl@idx), 
              levels = ap_om_5cl@exemplars,
              labels = paste0("cl_", 1:5))
seqdplot(seqdata = h2o_seq, 
         group = cl5, 
         main = "Distribution of communication intent over time",
         with.legend=FALSE, cpal = col_pallet)
# This is the best one so far, and looks interpreatable
# check also the frequency plot
c <- "cl_5"
seqfplot(h2o_seq[cl5==c,], 
         main = paste0("Seq. freq. plot:", c), 
         with.legend=FALSE, axes=F, idxs=1:20, cpal = col_pallet)
# This - ap_om_5cl - is the best solution I've managed to get with AP

