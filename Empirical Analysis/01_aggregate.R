
### combine all the data into one dataframe
d_combined = rbind(
    d_sim
  #, d_minson
  #, d_niederberger 
  # ...the rest
) %>%
### extract all individual-level analyses
mutate(
  extracted_var = estimate
)

## prep function for calculating S
calc_S = function(mu, sigma) {
  x=sort(x)
  mean(sapply(1:(length(x)-1), function(i){x[i+1]-x[i]}))/sigma
}

## prep function for calculating correlation in a group
## very brute force
get_cor = function(tr, tsk) {
  dx=d_combined %>%
    subset(trial==tr & task==tsk) %>%
    group_by(task, trial, participant) %>%
    mutate(
      task_index = 1:n()
    )%>%
    pivot_wider(id_cols=c(task, trial, participant), names_from=task_index, values_from = estimate) %>%
    ungroup %>%
    dplyr::select(-c(participant, task, trial))
  
  mycor = cor(dx)
  diag(mycor)=NA
  mean(mycor, na.rm=T)
}


### calculate group-level outcomes
d_group = d_combined %>%
  group_by(trial, task, task_index) %>%
  mutate(
      mu = mean(estimate)
    , med = median(estimate)
    , sd = sd(estimate)
  ) %>%
  group_by(trial, task) %>%
  summarize(
      S = calc_S(mu=unique(mu), sigma=mean(sd))
    , mean_choice = task_index[which.max(mu)]
    , med_choice = task_index[which.max(med)]
    , plurality_choice = names(table(choice))[which.max(table(choice))]
    , decoupled_mean_plurality = mean_choice != plurality_choice
    , decoupled_med_plurality = med_choice != plurality_choice
  ) %>%
  rowwise() %>%
  mutate(
    cor = get_cor(tr=trial, tsk=task)
  )
