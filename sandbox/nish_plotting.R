source("./src/helper_functions.R")

travel_probs <- read_rds("./testing/travel_probs.rds")
results_log <- read_csv("./results_log.csv")
ids <- subset(results_log, comm_version == 2) %>% select(unique_id)

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
  mutate(type = ifelse(type == "Urban", "City", "Non-city"),
         alpha_init = factor(alpha_init), 
         ld_b = factor(ld_b), 
         beta_inc = factor(beta_inc), 
         alpha_inc = factor(alpha_inc), 
         beta_dec = factor(beta_dec), 
         alpha_dec = factor(alpha_dec)) %>%
  left_join(travel_probs, by = "Community") -> out_data

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


