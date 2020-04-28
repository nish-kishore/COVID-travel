source("./src/helper_functions.R")

travel_probs <- read_rds("./testing/travel_probs.rds")
results_log <- read_csv("./results_log.csv")
ids <- results_log[17:nrow(results_log), "unique_id"]

lapply(1:nrow(ids), function(x) load_merge_vars(results_log, ids[x,], alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec, Nruns)) %>%
  bind_rows() -> plot_data

plot_data %>%
  group_by(Simulation)%>%
  summarise(min=min(t_ld_a)) %>%
  filter(min<1000) %>%
  pull(Simulation) -> non_fade_outs

plot_data %>%
  group_by(Community,Simulation, type, alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec) %>%
  summarise(infections = sum(n),
            start = min(DayInfected),
            t_ld_a = min(t_ld_a)) %>%
  ungroup() %>%
  group_by(Community, type, alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec) %>%
  summarise(prob_epi_1 = round(sum(infections>=1)/10,2),
            prob_epi_10 = round(sum(infections>=10)/10,2),
            prob_epi_30 = round(sum(infections>=30)/10,2),
            prob_epi_50 = round(sum(infections>=50)/10,2),
            av_start_time = round(mean(start - t_ld_a),1)) %>%
  ungroup() %>%
  mutate(type = factor(type),
         alpha_init = factor(alpha_init), 
         ld_b = factor(ld_b), 
         beta_inc = factor(beta_inc), 
         alpha_inc = factor(alpha_inc), 
         beta_dec = factor(beta_dec), 
         alpha_dec = factor(alpha_dec)) %>%
  left_join(travel_probs, by = "Community") -> out_data

create_plot <- function(ld_b2, beta_inc2, beta_dec2){
  out_data %>%
    subset(ld_b == ld_b2 & beta_inc == beta_inc2 & beta_dec == beta_dec2) %>%
    group_by(Community, type, alpha_inc, alpha_dec, prob_start_comm) %>%
    summarize(prob = mean(prob_epi_1)) %>%
    ungroup() %>%
    ggplot(aes(x = log(prob_start_comm), y = prob, color = type)) + 
    geom_point() + 
    geom_smooth(method = lm) + 
    facet_grid(alpha_dec~alpha_inc) + 
    theme_bw() + 
    labs(title = paste0("Delay: ", ld_b2, ", Beta(+): ",beta_inc2, ", Beta(-): ",beta_dec2), 
         x = "Log probability of travel from start community", y = "Probability of epidemic (> 1 case)",
         color = "Type")
}

out_data %>%
  select(ld_b, beta_inc, beta_dec) %>%
  distinct() -> data_types

for(i in 1:nrow(data_types)){
  in_data <- data_type[i,]
  p <- create_plot(in_data$ld_b, in_data$beta_inc, in_data$beta_dec)
  ggsave(paste0("./figs/scatter_plot_",in_data$ld_b,"-",in_data$beta_inc,"-",in_data$beta_dec,".png", dpi = 300))
}




#Regs
glm(prob_epi ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()

glm(av_start_time ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()


