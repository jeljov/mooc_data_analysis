## for the given student and the given posting data,
## the f. creates a sequence of post codes that describe the student's 
## posting activity. The f. returns a list of the following elements:
## - student_id
## - seq - a vector of post codes representing the sequence
create_stud_seq <- function(posts_data, stud_id) {
  
  stud_posts <- posts_data %>% filter(student_id == stud_id)
  n_posts <- nrow(stud_posts)
  seq_data <- list()
  seq_data$student_id <- stud_id
  ## vector to store sequence activities
  sequence <- vector()
  ## go through the student's posts
  current_code <- stud_posts$code[1]
  n_occurrence <- 1
  k <- 1
  for(j in 2:n_posts) {
    if ( stud_posts$code[j] == current_code ) {
        n_occurrence <- n_occurrence + 1
      }
      else {
        sequence[k] <- paste(current_code,"/",n_occurrence, sep = "")
        k <- k + 1
        current_code <- stud_posts$code[j]
        n_occurrence <- 1
      }
    ## if this is the last post
    if ( j == n_posts )
      sequence[k] <- paste(current_code,"/",n_occurrence, sep = "")
  }
  seq_data$seq <- sequence
    
  seq_data
}


## The f. create squences for the a particular course;
## that is, for all the students in the course
create_course_seq <- function(course_posts_sorted) {
  students <- unique(course_posts_sorted$student_id)
  course_seq_list <- list()
  for(stud_id in students) {
    course_seq_list <- append(course_seq_list, list(create_stud_seq(course_posts_sorted, stud_id)))
  }
  course_seq_list
}



## the f. first creates a new sequence list where all sequences will be
## of the same length - max_len_seq - by extending shorter sequences with NAs.
## Then, this list is transformed into a data frame, as required for the 
## creation of sequences in TraMineR
create_equal_length_seq <- function(sequence_list) { 
  # extract student ids into a vector
  students <- sapply(sequence_list, function(x) x[['student_id']])
  # extract sequences into a separate list
  sequences <- sapply(sequence_list, function(x) x[['seq']])
  # find the max length sequence
  max_len_seq <- max(sapply(sequences, length))
  # create a new sequence list where all sequences will be of the max_len_seq length
  eq_len_sequences <- list()
  for(i in 1:length(sequences)) {
    eq_len_sequences[[i]] <- sequences[[i]]
    if ( length(sequences[[i]]) < max_len_seq ) {
      d <- max_len_seq - length(sequences[[i]])
      eq_len_sequences[[i]] <- c(sequences[[i]], rep(NA, times=d))
    } 
  }
  # transform the eq_len_sequences list into a data frame
  # (from: http://stackoverflow.com/questions/4227223/r-list-to-data-frame?lq=1)
  seq_df <- data.frame(matrix(unlist(eq_len_sequences), 
                              nrow=length(eq_len_sequences), 
                              byrow=T),
                       stringsAsFactors=FALSE)
  seq_df$student_id <- students
  # set the student id to be the first variable
  last_var <- ncol(seq_df)
  seq_df <- seq_df[,c(last_var, 1:(last_var-1))]
  seq_df
}


# The function computes state-substitution costs via the 'FEATURES' 
# method of the seqcost() TraMineR's function.
# It uses a data frame with a few features describing the states 
# (communication intent types). 
# This approach might be considered subjective, though I've tried
# to base it fully on the characteristics of the 7 discussion intent
# types, as they are defined in the coding scheme
features_based_subs_cost <- function(thread_sequences) {
  state_features_df <- data.frame(state=attr(thread_sequences, "alphabet"),
                                  social=c(0,0,0,1,1,1,1),
                                  on_task=c(0,1,0,0,1,0,1),
                                  on_content=c(1,1,0,0,1,0,0))
  seqcost(seqdata = thread_sequences, 
                    method = "FEATURES", 
                    state.features = state_features_df)
}