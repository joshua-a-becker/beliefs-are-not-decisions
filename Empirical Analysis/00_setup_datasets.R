### this script collects all the simulated/empirical data 
### and arranges it into compatible structures

### Columns = trial, participant, task_identifier, estimate, choice


### load simulated data
d_sim = read.csv("../Theoretical Analysis/Sim Data/simulation_for_experiment.csv") %>%
  pivot_longer(cols=-c(participant, trial, task), names_to="task_index") %>%
  mutate(
    estimate=value
  ) %>%
  group_by(trial, participant, task)%>%
  mutate(
    choice=task_index[which.max(estimate)] # we need to artificially determine choice
                                           # for simulated data
  )


### load minson data
d_minson = read.csv()


### load niederberger data
d_niederberger = read.csv()
