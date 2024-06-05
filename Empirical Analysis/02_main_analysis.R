### generate visual overview figure here
### only one fig in this file---second fig needs a second file
d_group %>%
  mutate(
      S=round(S,1)
    , r=round(cor, 1)
    , decoupled = 1*decoupled_mean_plurality
  ) %>%
  ungroup %>%
  ggplot() + 
  stat_summary(fun="mean", geom="point", aes(x=S, y=decoupled), shape=1) + 
  stat_summary(fun="mean", geom="line", aes(x=S, y=decoupled)) + 
  stat_summary(fun="mean", geom="point", aes(x=r, y=decoupled))+
  stat_summary(fun="mean", geom="line", aes(x=r, y=decoupled))

  

### here, run statistical tests on key findings associated with this figure
### run additional statistical tests in another file