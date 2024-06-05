# read results from CSV
results = read.csv("simulation_results_corr.csv")


results_group = results %>%
    group_by(trial, S, M, N, r) %>%
  summarize(
    , mean = unique(mean)
    , median = unique(median)
    , plurality = unique(plurality)
    , two_stage = unique(two_stage)
    , ranked = unique(ranked)
  )

results_sum = results_group %>%
  mutate(
      mean_correct=mean==M
    # , med_correct=median==M
    , plur_correct=plurality==M
    , mean_plurality = mean==plurality
  ) %>%
  group_by(S,N,M,r) %>%
  summarize(
      mean_plurality = mean(mean!=plurality)
    , median_plurality = mean(median!=plurality)
    , mean_ranked = mean(mean!=ranked)
    , median_ranked = mean(median!=ranked)
    , mean_two_stage = mean(mean!=two_stage)
    , median_two_stage = mean(median!=two_stage)
    , mean_v_plur_mean_acc = mean(mean_correct[mean!=plurality])
    , mean_v_plur_plur_acc = mean(plur_correct[mean!=plurality])
    , mean_v_plur_disagree = mean(plurality!=mean)
    # , mean_v_plur_vote_acc
  ) %>%
  mutate(
    M_lab = paste0("M=", M)
  )


results_sum

# Plotting
my_labeller = function(variable, value){
  unique(results_sum$M_lab[results_sum$M==value])
}

my_labeller_N = function(variable, value){
  unique(results_sum$M_lab[results_sum$N==value])
}


plot_metric=function(metric, color_guide=F){
  ggplot(results_sum, 
         aes(x = r, y = eval(parse(text=metric)), color = factor(M), group = interaction(factor(M), factor(N)))
         ) +
    geom_line() +
    facet_grid(S~N) +
    theme_minimal(base_size = 16) +
    theme(panel.spacing = unit(1.5, "lines"))+ 
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(c(0,1))+xlim(c(0,1))
}
  

plot_metric("mean_plurality", color_guide=NULL)
#ggsave("probability_color_guide.png", width=8.2, height=6.5)


plot_metric("mean_plurality")
#ggsave("probability_mean_plurality.png", width=8.2, height=6.5)
##

plot_metric("median_plurality")
#ggsave("probability_median_plurality.png", width=8.2, height=6.5)


plot_metric("mean_ranked")
#ggsave("probability_mean_ranked.png", width=8.2, height=6.5)

plot_metric("median_ranked")
#ggsave("probability_median_ranked.png", width=8.2, height=6.5)

plot_metric("mean_two_stage")
ggsave("probability_mean_two_stage.png", width=8.2, height=6.5)

plot_metric("median_two_stage")
ggsave("probability_median_two_stage.png", width=8.2, height=6.5)


## PLOTTING ACCURACY OF MEAN VS VOTE

results_sum %>%
  ggplot(aes(x=S)) +
  geom_line(aes(y=mean_v_plur_mean_acc), color="blue") +
  geom_line(aes(y=mean_v_plur_plur_acc), color="grey") +
  facet_grid(N~M) +
  labs(
    y="% Correct Predictions"
    , title = "Accuracy When Mean & Vote (Plur.) Disagree\nBlue=Mean, Green=Vote"
  )
                     

results_sum %>%
  subset(N==20 & M==10) %>%
  ggplot(aes(x=S)) +
  geom_line(aes(y=mean_v_plur_mean_acc), color="blue") +
  geom_line(aes(y=mean_v_plur_plur_acc), color="black") +
  geom_line(aes(y=mean_v_plur_disagree), color="#999999", linetype="dashed") +
  geom_text(aes(label="Mean", x=0.85, y=0.9), color="blue")+
  geom_text(aes(label="Vote (Plurality)", x=0.32, y=0.04))+
  geom_text(aes(label="Prob(Disagree)", x=0.48, y=0.43), color="#666666")+
  labs(
    y="Accuracy/Probability"
    , x="Step"
    , title = "Accuracy When\nMean & Vote Disagree"
  ) + 
  theme(plot.title = element_text(size=12)) +
  theme_bw()

ggsave("Accuracy_Mean_v_Plurality.png", width=2.8, height=2.8)

