source("./src/helper_functions.R")

travel_probs <- read_rds("./testing/travel_probs.rds")
results_log <- read_csv("./results_log.csv")
ids <- subset(results_log, comm_version == 3 & a0==1) %>% dplyr::select(unique_id)

lapply(1:nrow(ids), function(x) load_merge_vars(results_log, ids[x,], cases_ld_a, beta_inc, alpha_inc, beta_dec, alpha_dec, Nruns)) %>%
  bind_rows() -> plot_data

plot_point_comp <- function(day_till, cases_ld){
  plot_data %>%
    subset(cases_ld_a %in% c(9999, cases_ld) & beta_inc == 1.5 & beta_dec == 0.5 & alpha_dec == 0.5) %>% 
    mutate(sim_type = case_when(
      cases_ld_a == 9999 ~ "Control", 
      alpha_inc == 1 ~ "Lockdown-No Surge", 
      alpha_inc == 2 ~ "Lockdown-Travel Surge"
    )) %>%
    arrange(cumulative) %>%
    subset(cumulative >= day_till) %>%
    group_by(Community,Simulation, sim_type) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(Community) %>%
    mutate(nsims=length(n)) %>%
    ungroup() %>%
    group_by(Community, sim_type) %>%
    summarise(avg_day_to_n = sum(DayInfected)/mean(nsims)) %>%
    ungroup() -> out_data
  
  subset(out_data, sim_type == "Control") %>%
    dplyr::select(Community, avg_day_to_n) %>%
    rename("ctrl_day_to_n" = "avg_day_to_n") %>%
    right_join(out_data, by = "Community") %>%
    ggplot(aes(x = ctrl_day_to_n, y = avg_day_to_n, color = sim_type)) +
    geom_point() + 
    geom_smooth(method = "loess", se = F) + 
    theme_bw() + 
    labs(x = "Control - Avg days till X cases", y = "Comparison - Avg days till X cases",
         title = paste0("Avg # of days till ",day_till, " cases / Lockdown after: ", cases_ld, " cases"), 
         color = "Sim Type")
}

plot_point_comp(10, 30)



create_plot <- function(data, alpha_inc2, ld_b2, cases){
  data %>%
    subset(ld_b == ld_b2 & alpha_inc == alpha_inc2) %>%
    rename("prob" = paste0("prob_epi_",cases)) %>%
    group_by(Community, type, beta_inc, beta_dec, prob_start_comm) %>%
    summarize(prob = mean(prob)) %>%
    ungroup() %>%
    ggplot(aes(x = log(prob_start_comm), y = prob)) + 
    geom_point() + 
    geom_smooth(method = "loess") + 
    facet_grid(beta_dec~beta_inc) + 
    theme_bw() + 
    labs(title = paste0("Delay: ", ld_b2, " days, Alpha(+): ",alpha_inc2,", Alpha(-): 0.5"), 
         x = "Log probability of travel from start community", 
         y = paste0("Probability of epidemic (> ",cases," cases)"))
}


ggarrange(create_plot(out_data, 5, 3, 10), create_plot(out_data, 5, 7, 10), ncol = 1)




#Regs
glm(prob_epi ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()

glm(av_start_time ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()




# get number of communities that have X cases by Y date in any simulations
num_comms<- function(day_till,day,cases_ld){
  plot_data %>%
    subset(cases_ld_a %in% c(9999, cases_ld) & beta_inc == 1.5 & beta_dec == 0.5 & alpha_dec == 0.5) %>% 
    mutate(sim_type = case_when(
      cases_ld_a == 9999 ~ "Control", 
      alpha_inc == 1 ~ "Lockdown-No Surge", 
      alpha_inc == 2 ~ "Lockdown-Travel Surge"
    )) %>%
    arrange(cumulative) %>%
    subset(cumulative >= day_till & DayInfected<=day) %>%
    group_by(Community,sim_type) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(sim_type) %>%
    summarise(ncomm=length(Community)) -> out_data
  
  return(out_data)
}

results <- NULL
for (i in 1:30){ # cases, day, 
  for (j in 1:60){
    cat(i,j,"\n")
    out <- num_comms(i,j,30)
    results <-rbind(results,cbind(out,
                            rep(i,nrow(out)),
                                  rep(j,nrow(out))))
  }
}


results %>%
  #filter(sim_type!="control") %>%
  ggplot() + geom_line(aes(x=j,y=ncomm,color=factor(sim_type))) + xlim(0,30)+
  facet_wrap(vars(i)) + theme_bw()+
  labs(x="# days",
       y="# communities",
       title="# of communities by simulation type with X cases by each day",
       color=element_blank(),
       caption="Trigger: 30 cases")

#KM Curves
int_results_log <- results_log %>%
  mutate(flag  = case_when(
    alpha_inc == 1 & beta_inc == 1 & alpha_dec == 1 & beta_dec == 1 & ld_b == 3 ~ "No Lockdown", 
    alpha_inc == 3 & beta_inc == 2 & alpha_dec < 1 & beta_dec < 1 & ld_b == 3 ~ "Large Surge", 
    alpha_inc == 2 & beta_inc == 1.5 & alpha_dec < 1 & beta_dec < 1 & ld_b == 3 ~ "Small Surge", 
    alpha_inc == 1 & beta_inc == 1 & alpha_dec < 1 & beta_dec < 1 & ld_b == 0 ~ "Lockdown - no Surge", 
    T ~ "Other")
    ) %>%
  subset(flag != "Other" & alpha_init == 0.01 & cases_ld_a == 30)

lapply(int_results_log$unique_id, function(x) load_merge_vars(int_results_log, x, Nruns, flag)) %>%
  bind_rows() -> km_plot_data


x <- km_plot_data %>%
  arrange(flag, Simulation, DayInfected) %>%
  group_by(flag, Simulation, Community) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(flag, Simulation, DayInfected) %>%
  summarise(comm_inf = n())

x %>%
  mutate(tot_inf = cumsum(comm_inf)/100) %>%
  mutate(flag = factor(flag, levels = c("No Lockdown", "Large Surge", "Small Surge", "Lockdown - no Surge"))) %>%
  ungroup() %>%
  group_by(DayInfected, flag) %>%
  summarise(prop_inf = mean(tot_inf)) %>%
  subset(DayInfected <= 30) %>%
  ggplot(aes(x = DayInfected, y = prop_inf, group = flag, color = flag)) + 
  geom_line(size = 1.05) + 
  theme_bw() + 
  labs(x = "Day", y = "Proportion of communities with an imported case", color = "Type of Lockdown")

ggsave("./figs/prop_comm_import.png", dpi = 300)
