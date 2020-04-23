#scripts to generate plots
source("./src/dependencies.R")
source("./src/helper_functions.R")

#load log of results generated so far 
results_log <- read_csv("./results_log.csv")
row <- 3

results_master <- load_run_results(results_log[row,"unique_id"])
params <- results_log[row,] %>% as.list()

#overall
# results_master %>%
#   filter(!is.na(Community)) %>%
#   group_by(Community, Simulation) %>%
#   summarise(n=n()) -> community_summary

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
  group_by(DayInfected, Simulation, Community, type, t_ld_a) %>%
  summarise(n=n()) %>%
  group_by(Community, Simulation) %>%
  mutate(cumulative=cumsum(n)) -> community_day_summary

# results_master %>%
#   filter(!is.na(Community)) %>%
#   group_by(DayInfected, Simulation, type) %>%
#   summarise(n=n()) %>%
#   group_by(type, Simulation) %>%
#   mutate(cumulative=cumsum(n)) -> community_type_summary
# 
# community_type_summary %>%
#   #ungroup() %>%
#   group_by(DayInfected, type,Simulation) %>%
#   #can change to median once we have more runs
#   #summarise(avg_cumulative = mean(cumulative)) %>%
#   ggplot(aes(x=DayInfected,y=cumulative,color=factor(type),lty=factor(Simulation))) +
#   scale_color_viridis_d() +
#   geom_line() + theme_classic() + 
#   labs(color="Community",
#        lty = "Simulation",
#        x="Days",
#        y="Cumulative cases",
#        caption = paste0("Relative increase in travel post lockdown announcement = ", params$alpha_inc,
#                         "\n Average total # cases = ", sum(community_type_summary$n)/max(community_type_summary$Simulation),
#                         "\n # simulations = ", params$Nruns))

num_edge <- sqrt(params$num_communities)
row <- c(rep(1:num_edge,each=10))
col <- c(rep(1:num_edge,times=10))

urban <- c(45,57)
suburban <-c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
rural <- setdiff(1:num_communities, c(urban, suburban))

# summarise total # infections and start times by community 
results_master %>%
  group_by(Community,Simulation, type, t_ld_a) %>%
  summarise(infections = sum(n),
            start = min(DayInfected)) %>%
  group_by(Community, type) %>%
  summarise(prob_epi = round(sum(infections>=params$cases_ld_a)/params$Nruns,2),
            av_start_time = mean(start - t_ld_a)) %>%
  mutate(row = row[Community],
         col = col[Community]) -> summary_stats

excluded <- setdiff(1:100,summary_stats$Community)
type_excluded <- ifelse(excluded %in% urban, "urban",
                        ifelse(excluded %in% suburban, "suburban", rural))

bind_cols("Community"=excluded,
      "type"=type_excluded,
      "prob_epi"=rep(0,length(excluded)),
      "av_start_time"=rep(NA,length(excluded)),
      "row"=row[excluded],
      "col"=col[excluded]) %>%
        as_tibble() %>%
          bind_rows(summary_stats)-> summary_stats_complete

heatmap1 <- ggplot(summary_stats_complete,aes(x=col,y=row)) + 
  geom_tile(aes(fill=prob_epi,color=type),size=2,width=0.8,height=0.8) + 
  scale_color_grey()+
  geom_text(aes(label = round(av_start_time))) +
  scale_fill_viridis_c(option="plasma",limits=c(0,1)) + theme_classic() + 
  theme(legend.position = "bottom", axis.ticks = element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.line = element_blank()) + 
  labs(fill="Probability epidemic",
       color=element_blank(),
       caption="Number in cell denotes average time of first case \nrelative to announcement of restrictions") 


# add summary_stats back to results_log
# try(results_log[row,"rural %" ] <- summary_stats[summary_stats$type=="rural","perc_infections"],silent=T)
# try(results_log[row,"suburban %" ] <- summary_stats[summary_stats$type=="suburban","perc_infections"],silent=T)
# try(results_log[row,"urban %" ] <- summary_stats[summary_stats$type=="urban","perc_infections"],silent=T)
# try(results_log[row,"rural start time" ] <- summary_stats[summary_stats$type=="rural","start_time"],silent=T)
# try(results_log[row,"suburban start time" ] <- summary_stats[summary_stats$type=="suburban","start_time"],silent=T)
# try(results_log[row,"urban start time" ] <- summary_stats[summary_stats$type=="urban","start_time"],silent=T)
# results_log[row,"average cases" ] <-  sum(community_day_summary$n)/params$Nruns



# correlation of time series
# community_day_summary %>%
#   select(Community,DayInfected,Simulation,n) %>%
#   group_by(Simulation) %>%
#   spread(DayInfected,n)-> time_series
# 
# Correlations <- rep(NA,params$Nruns)
# for (i in 1:params$Nruns){
#   time_series  %>%
#     subset(Simulation ==i) %>%
#     as.matrix() %>%
#     mSynch(na.rm=TRUE) -> Correlations[i]
# }
# 
# Correlations %>%
#   unlist() %>%
#   mean() -> results_log[row,"correlation" ] 
# 
# write.csv(results_log,"./results_log.csv",row.names = FALSE)


