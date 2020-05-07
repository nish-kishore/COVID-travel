source("./src/helper_functions.R")

travel_probs <- read_rds("./testing/travel_probs.rds")
results_log <- read_csv("./results_log.csv")
ids <- subset(results_log, comm_version == 3 & a0==1 & cases_ld_a2 == 1) %>% dplyr::select(unique_id)

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
    out <- num_comms(i,j,10)
    results <-rbind(results,cbind(out,
                            "i"=rep(i,nrow(out)),
                                "j"=rep(j,nrow(out))))
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
       caption="Trigger: 30 cases; note this is # communities that show up in ANY simulation")



num_comms2<- function(day_till,day,rds_id_control,rds_id_nosurge,rds_id_surge){

    data1 <- read_rds(paste0("./cache/results/",rds_id_control,".rds"))
    data1 %>%
      mutate(sim_type="Control") -> data1
    data2 <- read_rds(paste0("./cache/results/",rds_id_nosurge,".rds"))
    data2 %>%
      mutate(sim_type="Lockdown-No Surge") -> data2
    data3 <- read_rds(paste0("./cache/results/",rds_id_surge,".rds"))
    data3 %>%
      mutate(sim_type="Lockdown-Travel Surge") -> data3
    
    data <- rbind(data1,data2,data3)
    
    data %>%
      arrange(cumulative) %>%
      subset(cumulative >= day_till & DayInfected<=day) %>%
      group_by(Community,sim_type,Simulation) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(sim_type,Simulation) %>%
      summarise(ncomm=length(Community)) %>%
      group_by(sim_type) %>%
      summarise(mean_comm=sum(ncomm)/50)-> out_data
  
  return(out_data)
}


num_comms2(10,60,
           "c35e97ec40811d7ff7161a394c194eac",
           "a320c39d1afc8b11a9800ff98cc0128a",
           "49dcd91e5338350fd7b1d50417f76f90")





