library(tidyverse)
library(ggtext)
library(MASS)


# ranks=replicate(N, sample(1:10,10))%>%t
rank_voting = function(beliefs, N){
  
  
  ranks = t(apply(beliefs, 1, rank))
  vote_table = table(factor(apply(ranks, 1, which.max), levels=1:dim(beliefs)[2]))/N
  
  if(vote_table[which.max(vote_table)]>0.5) {
    return(as.numeric(names(which.max(vote_table))))
  }
  
  if(vote_table[which.max(vote_table)]==0.5 & sum(vote_table>0) == 2) {
    return(as.numeric(names(vote_table))[sample(1:2,1)])
  }
  
  
  
  sorted_votes=sort(vote_table)
  
  
  
  beliefs[,as.numeric(names(sorted_votes[sorted_votes!=0])[1])]=min(beliefs)-100000
  
  # return(rank_voting(beliefs[,-as.numeric(names(sort(vote_table))[1])], N))
  return(rank_voting(beliefs,N))
}



two_stage = function(beliefs, N) {
  individual_votes <- apply(beliefs, 1, which.max)
  vote_table = table(factor(individual_votes, levels=1:dim(beliefs)[2]))/N
  
  if(vote_table[which.max(vote_table)]>0.5) {
    return(as.numeric(names(which.max(vote_table))))
  }
  
  new_vote_table=table(apply(beliefs[,as.numeric(names(rev(sort(vote_table))[1:2]))], 1, which.max))
  as.numeric(names(rev(sort(vote_table))[1:2]))[as.numeric(names(new_vote_table[which.max(new_vote_table)]))]
}


simulate_outcomes <- function(S, N, M, r, num_simulations) {
  #print(paste0("\nS=",S, ", N=", N, ", M=", M, ", r=", r))
  results <- lapply(1:num_simulations, function(i){
    sigma=matrix(rep(r, M*M), nrow=M)
    diag(sigma)=1
    mu=rep(S * (0:(M-1)))
    beliefs <- mvrnorm(n=N, mu=mu, Sigma=sigma)
    
    # Averaging Choices
    mean_choice <- which.max(colMeans(beliefs))
    median_choice <- which.max(apply(beliefs, 2, median))
    
    # Voting Choices
    individual_votes <- apply(beliefs, 1, which.max)
    plurality_choice <- as.numeric(names(which.max(table(factor(individual_votes, levels=1:M)))))
    # Two-Stage Voting
    two_stage_choice <- two_stage(beliefs, N)
    
    # Ranked Voting (simplified version)
    ranked_choice <- rank_voting(beliefs, N)
    
    # Comparing Choices
    data.frame(trial=i, mean = mean_choice, median = median_choice, plurality = plurality_choice, 
      two_stage = two_stage_choice, ranked = ranked_choice, beliefs=beliefs)
    
    }) %>%
    do.call(rbind, .)
  
  results
}

# Parameters
S_values <- 0.1#seq(0.1, 0.5, by = 0.2)
N_values <- 20#rev(c(5, 20, 50))
M_values <- 10#rev(c(5, 10))
r_values = 0#c(0,0.25,0.5,0.75,1)
num_simulations <- 100

# Running simulations
results <- expand.grid(M = M_values, S = S_values, N = N_values, r=r_values) %>%
  group_by(M,S,N,r) %>%
  do(
    simulate_outcomes(S=.$S, N=.$N, M=.$M, r=.$r, num_simulations=num_simulations)
  )

# Save results to CSV
# write.csv(results, "Sim Data/simulation_results.csv", row.names = FALSE)

