# f. generates summary statistics aimed at giving some insight into the clusters
# the 1st parameter is the feature set to be used for cluster comparison; it is
# assumed that all features in the set are numerical;
# the 2nd parameter is a vector with cluster identifiers 
# (indicating the cluster each student is assigned to)
summary.stats <- function(feature_set, clusters) {
  sum_stats <- aggregate(x = feature_set, 
                         by = list(clusters), 
                         FUN = function(x) { 
                           q <- quantile(x, probs = c(0.25, 0.5, 0.75), names = F, na.rm = T)
                           paste(round(q[2], digits = 2), "; (", 
                                 round(q[1], digits = 2), ", ",
                                 round(q[3], digits = 2), ")", sep = "")
                         })
  sum_stat_df <- data.frame(cluster = sum_stats[,1], 
                            freq = as.vector(table(clusters)),
                            sum_stats[,-1])
  
  sum_stats_transpose <- t( as.matrix(sum_stat_df) )
  sum_stats_transpose <- as.data.frame(sum_stats_transpose)
  attributes <- rownames(sum_stats_transpose)
  sum_stats_transpose <- as.data.frame( cbind(attributes, sum_stats_transpose) )
  cl_number <- max(as.integer(clusters))
  colnames(sum_stats_transpose) <- c( "attributes", rep("Median; (Q1, Q3)", cl_number) )
  rownames(sum_stats_transpose) <- NULL
  
  sum_stats_transpose
  
}


pairwise.compare.Mann.Whitney <- function(df, f_var, cl_var, nclust) {
  # compute the number of comparison
  ncompare <- 0
  for(i in 1:(nclust-1)) ncompare <- ncompare + i
  # make comparisons  
  fe_comparison <- matrix(nrow = ncompare, ncol = 5, byrow = T)
  k <- 1
  for(i in 1:(nclust-1)) {
    for(j in (i+1):nclust) {
      fe_comparison[k,] <- c(i, j, feature.compare.Mann.Whitney(df, f_var, cl_var, i, j))
      k <- k+1
    }
  }
  fe_comparison_df <- as.data.frame(fe_comparison)
  colnames(fe_comparison_df) <- c('c1', 'c2', 'Z', 'p', 'effect.size')
  ## apply the FDR correction to the comparisons
  apply.FDR.correction(fe_comparison_df)
}


## f. for performing Mann-Whitney U Test on the given feature (2nd parameter) 
## of the given data frame (1st parameter). The last two parameters define the   
## groups to be compared 
feature.compare.Mann.Whitney <- function(df, feature, group, g1, g2) {
  g1_vals <- subset(df[[feature]], df[[group]] == g1)
  g2_vals <- subset(df[[feature]], df[[group]] == g2)
  do.Mann.Whitney.test(g1_vals, g2_vals, g1, g2)
}


## f. for executing Mann-Whitney U Test
do.Mann.Whitney.test <- function(g1_vals, g2_vals, group1, group2) {
  require(coin)
  g <- factor(c(rep(group1, length(g1_vals)), rep(group2, length(g2_vals))))
  v <- c(g1_vals, g2_vals)
  w <- wilcox_test(v ~ g, distribution="exact")
  z_value <- round(statistic(w)[[1]], digits = 4)
  n <- length(g1_vals) + length(g1_vals)
  r <- round(abs(z_value)/sqrt(n), digits = 4)
  c(Z=z_value, p=round(pvalue(w), digits = 6), effect.size=r)
}


## f. applies FDR correction on the results in the input data frame
## and adds a new column indicating if the result is significant or not
## order the comparisons data based on the p value
## to prepare it for applying the FDR correction
apply.FDR.correction <- function(results.df) {
  results.df <- results.df[order(results.df$p),]
  ## deterine significance using the FDR correction
  alpha <- 0.05
  n.results <- nrow(results.df)
  results.df$significant <- vector(length = n.results)
  results.df$alpha <- vector(length = n.results)
  for (i in 1:n.results) {
    alpha.adjusted <- (i/n.results)*alpha 
    if ( results.df$p[i] <=  alpha.adjusted ) { 
      results.df$significant[i] <- 'YES'
      results.df$alpha[i] <- alpha.adjusted
    } else {
      for (j in i:n.results) {
        results.df$significant[j] <- 'NO'
        results.df$alpha[j] <- (j/n.results)*alpha
      }
      break
    }
  }
  results.df
}



