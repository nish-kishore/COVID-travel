source("./src/helper_functions.R")

travel_probs <- read_rds("./testing/travel_probs.rds")
results_log <- read_csv("./results_log.csv")

analysis_ids <- c("9b6827e0d3c999bd79a48c7622ebcd79",
                  "e5511f2531fb8f693a548fba6b884984",
                  "dcf3558c667a1ee8be443d280c8a0130",
                  "d5f5f2a80db152665949c80ac139d051",
                  "236d19fad8a7295ec9939d47c0331f88",
                  "5dcd7cadf92ba0de2099eab0d63a2cf3",
                  "75994176f2355e1babb8fdb9bf142d26")

ids <- subset(results_log, unique_id %in% analysis_ids) %>% dplyr::select(unique_id)


lapply(1:nrow(ids), function(x) load_merge_vars(results_log, ids[x,], cases_ld_a, beta_inc, alpha_inc, beta_dec, alpha_dec, Nruns)) %>%
  bind_rows() %>%
  mutate(n=1)-> plot_data


plot_data %>%
  group_by(Simulation, DayInfected, Community, type, cases_ld_a, alpha_inc) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(Simulation = as.numeric(Simulation)) %>%
  dplyr::select(-type) -> plot_data_summary 

plot_data_summary %>%
  complete(Simulation, DayInfected, Community,cases_ld_a, alpha_inc, fill = list(n = 0)) %>%
  group_by(Simulation, Community, cases_ld_a, alpha_inc) %>%
  mutate(cumulative=cumsum(n)) %>%
  filter(!(cases_ld_a==9999 & alpha_inc!=1)) -> plot_data_summary

plot_point_comp <- function(day_till, cases_ld){
  plot_data_summary%>%
    subset(cases_ld_a %in% c(cases_ld)) %>%
    mutate(sim_type = case_when(
      #cases_ld_a == 9999 ~ "Control",
      alpha_inc == 1 ~ "Lockdown-No Surge",
      alpha_inc == 2 ~ "Lockdown-Travel Surge2",
      alpha_inc == 3 ~ "Lockdown-Travel Surge3"
    )) %>%
    mutate(DayInfected = case_when(
      cumulative==0 ~ 60,
      TRUE ~ DayInfected),
    cumulative = case_when(
      cumulative==0 ~ 10000,
      TRUE ~ cumulative
    )) %>%
    arrange(cumulative) %>%
    subset(cumulative >= day_till) %>%
    group_by(Community,Simulation, cases_ld_a, sim_type) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(Community, sim_type) %>%
    summarise(avg_day_to_n = sum(DayInfected)/50) %>%
    ungroup() -> out_data

  subset(out_data, sim_type == "Lockdown-No Surge") %>%
    dplyr::select(Community, avg_day_to_n) %>%
    rename("ctrl_day_to_n" = "avg_day_to_n") %>%
    right_join(out_data, by = "Community") %>%
    ggplot(aes(x = ctrl_day_to_n, y = avg_day_to_n, color = sim_type)) +
    geom_point() +
    geom_smooth(method = "loess", se = F) +
    theme_bw() +
    labs(x = paste0("Control - Avg days until", day_till, "case(s)"), y = paste0("Comparison - Avg days until", day_till, "case(s)"),
         title = paste0("Avg # of days until ",day_till, " case(s) / Lockdown after: ", cases_ld, " cases"),
         color = "Sim Type")
}

plot_point_comp(1, 30)



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
  plot_data_summary%>%
    subset(cases_ld_a %in% c(9999, cases_ld)) %>% 
    mutate(sim_type = case_when(
      cases_ld_a == 9999 ~ "Control", 
      alpha_inc == 1 ~ "Lockdown-No Surge", 
      alpha_inc == 2 ~ "Lockdown-Travel Surge2",
      alpha_inc == 3 ~ "Lockdown-Travel Surge3"
    )) %>%
    arrange(cumulative) %>%
    subset(cumulative >= day_till & DayInfected<=day) %>%
    group_by(Simulation,Community,sim_type) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(sim_type,Simulation) %>%
    summarise(ncomm=length(Community)) %>%
    group_by(sim_type) %>%
    summarise(mean_comm=mean(ncomm)) -> out_data
  
  return(out_data)
}

# cases <- c(1,2,5,10,25,50)
# results10 <- NULL
# for (i in 1:length(cases)){ # cases, day, 
#   for (j in 1:60){
#     cat(cases[i],j,"\n")
#     out <- num_comms(cases[i],j,10)
#     results10 <-rbind(results10,cbind(out,
#                             "case"=rep(cases[i],nrow(out)),
#                                 "day"=rep(j,nrow(out))))
#   }
# }



results10 %>%
  #filter(!(sim_type %in% c("Control","Lockdown-Travel Surge2"))) %>%
  ggplot() + geom_line(aes(x=day,y=mean_comm,color=factor(sim_type))) + xlim(0,60)+
  facet_wrap(vars(case)) + theme_bw()+
  labs(x="# days",
       y="# communities",
       title="# of communities by simulation type with X cases by each day",
       color=element_blank(),
       caption="Trigger: 10 cases")


# cases <- c(1,2,5,10,25,50)
# results30 <- NULL
# for (i in 1:length(cases)){ # cases, day, 
#   for (j in 1:60){
#     cat(cases[i],j,"\n")
#     out <- num_comms(cases[i],j,30)
#     results30 <-rbind(results30,cbind(out,
#                                       "case"=rep(cases[i],nrow(out)),
#                                       "day"=rep(j,nrow(out))))
#   }
# }




results30 %>%
  #filter(!(sim_type %in% c("Control","Lockdown-Travel Surge2"))) %>%
  ggplot() + geom_line(aes(x=day,y=mean_comm,color=factor(sim_type))) + xlim(0,60)+
  facet_wrap(vars(case)) + theme_bw()+
  labs(x="# days",
       y="# communities",
       title="# of communities by simulation type with X cases by each day",
       color=element_blank(),
       caption="Trigger: 30 cases")

plot_data_summary %>%
  filter(cases_ld_a %in% c(10)) %>%
  mutate(sim_type = case_when(
    cases_ld_a == 9999 ~ "Control",
    alpha_inc == 1 ~ "Lockdown-No Surge",
    alpha_inc == 2 ~ "Lockdown-Travel Surge2",
    alpha_inc == 3 ~ "Lockdown-Travel Surge3"
  )) %>%
  group_by(sim_type,Community,DayInfected) %>%
  summarise(mean=sum(cumulative)/50) %>%
  ggplot() + 
  geom_line(aes(x=DayInfected,y=mean,color=factor(sim_type))) + 
  facet_wrap(vars(Community),scales="free_y") + 
  labs(color=element_blank(),
       x="Day",
       y="Average cumulative cases",
       caption="Trigger = 10 cases")






