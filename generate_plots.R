#scripts to generate plots
source("./src/dependencies.R")
source("./src/helper_functions.R")

#load log of results generated so far 
results_log <- read_csv("./results_log.csv")

results_master <- load_run_results(results_log[1,"unique_id"])
params <- results_log[1,] %>% as.list()

#overall
results_master %>%
  filter(!is.na(Community)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary

#pre lockdown announcement summary + lag for inc period
results_master %>%
  filter(!is.na(Community) & DayInfected < (params$t_ld_a+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_prelockdown

results_master %>%
  filter(!is.na(Community) & DayInfected >=(params$t_ld_a+5) & DayInfected <= (params$t_ld_b+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_postlockdown_a

results_master %>%
  filter(!is.na(Community) & Simulation & DayInfected >=(params$t_ld_b+5)) %>%
  group_by(Community) %>%
  summarise(n=n()) -> community_summary_postlockdown_b

# add summary statistics for results_master -- time to X # infections, distribution across communities, etc

results_master %>%
  filter(!is.na(Community)) %>%
  group_by(DayInfected, Community, Simulation) %>%
  summarise(n=n()) %>%
  group_by(Community) %>%
  mutate(cumulative=cumsum(n)) -> community_day_summary

community_day_summary %>%
  ungroup() %>%
  group_by(DayInfected, Community) %>%
  #can change to median once we have more runs
  summarise(avg_cumulative = mean(cumulative)) %>%
  ggplot(aes(x=DayInfected,y=avg_cumulative,group=factor(Community),color=factor(Community))) +
  geom_line() + theme_classic() +
  labs(color="Community",
       x="Days",
       y="Cumulative cases",
       caption = paste0("Starting community = ",params$start_comm,
                        "\n Total # cases = ", sum(community_day_summary$n),
                        "\n Vertical black lines indicate when lockdown was announced and began"))
