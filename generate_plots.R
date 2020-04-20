#scripts to generate plots
source("./src/dependencies.R")
source("./src/helper_functions.R")

#load log of results generated so far 
results_log <- read_csv("./results_log.csv")
row <- 16

results_master <- load_run_results(results_log[row,"unique_id"])
params <- results_log[row,] %>% as.list()

#overall
results_master %>%
  filter(!is.na(Community)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary

# #pre lockdown announcement summary + lag for inc period
# results_master %>%
#   filter(!is.na(Community) & DayInfected < (params$t_ld_a+5)) %>%
#   group_by(Community, Simulation) %>%
#   summarise(n=n()) -> community_summary_prelockdown
# 
# results_master %>%
#   filter(!is.na(Community) & DayInfected >=(params$t_ld_a+5) & DayInfected <= (params$t_ld_b+5)) %>%
#   group_by(Community, Simulation) %>%
#   summarise(n=n()) -> community_summary_postlockdown_a
# 
# results_master %>%
#   filter(!is.na(Community) & Simulation & DayInfected >=(params$t_ld_b+5)) %>%
#   group_by(Community) %>%
#   summarise(n=n()) -> community_summary_postlockdown_b

# add summary statistics for results_master -- time to X # infections, distribution across communities, etc

results_master %>%
  filter(!is.na(Community)) %>%
  group_by(DayInfected, Simulation, Community, type) %>%
  summarise(n=n()) %>%
  mutate(cumulative=cumsum(n)) -> community_day_summary

results_master %>%
  filter(!is.na(Community)) %>%
  group_by(DayInfected, Simulation, type) %>%
  summarise(n=n()) %>%
  group_by(type) %>%
  mutate(cumulative=cumsum(n)) -> community_type_summary

community_type_summary %>%
  #ungroup() %>%
  group_by(DayInfected, type,Simulation) %>%
  #can change to median once we have more runs
  #summarise(avg_cumulative = mean(cumulative)) %>%
  summarise(cumulative_type = sum(cumulative)) %>%
  ggplot(aes(x=DayInfected,y=cumulative_type,color=factor(type),lty=factor(Simulation))) +
  scale_color_viridis_d() +
  geom_line() + theme_classic() + 
  labs(color="Community",
       lty = "Simulation",
       x="Days",
       y="Cumulative cases",
       caption = paste0("Relative increase in travel post lockdown announcement = ", params$alpha_inc,
                        "\n Average total # cases = ", sum(community_type_summary$n)/max(community_type_summary$Simulation),
                        "\n # simulations = ", max(community_type_summary$Simulation)))

# summarise total # infections and start times by community type
community_day_summary %>%
  group_by(Community,Simulation,type) %>%
  summarise(infections = sum(n),
            start = min(DayInfected)) %>%
  group_by(type) %>%
  summarise(tot_infections = sum(infections),
            start_time = mean(start)) %>%
  #group_by(Simulation) %>%
  mutate(perc_infections = tot_infections / sum(tot_infections)) -> summary_stats

# add summary_stats back to results_log? 

results_log[row,"rural %" ] <- summary_stats[1,"perc_infections"]
results_log[row,"suburban %" ] <- summary_stats[2,"perc_infections"]
results_log[row,"urban %" ] <- summary_stats[3,"perc_infections"]
results_log[row,"total cases" ] <- sum(community_day_summary$n)

write.csv(results_log,"./results_log.csv",row.names = FALSE)


