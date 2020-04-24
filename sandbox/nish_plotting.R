source("../src/helper_functions.R")

results_log <- read_csv("./results_log.csv")
ids <- results_log[17:nrow(results_log), "unique_id"]

lapply(1:nrow(ids), function(x) load_merge_vars(results_log, ids[x,], alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec, Nruns)) %>%
  bind_rows() -> plot_data

plot_data %>%
  subset(cumulative >= 10) %>%
  dplyr::select(Community, cumulative)
  
plot_data %>%
  group_by(Community,Simulation, type, alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec) %>%
  summarise(infections = sum(n),
            start = min(DayInfected),
            t_ld_a = min(t_ld_a)) %>%
  ungroup() %>%
  group_by(Community, type, alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec) %>%
  summarise(prob_epi = round(sum(infections>=10)/10,2),
            av_start_time = round(mean(start - t_ld_a),1)) -> out_data

glm(prob_epi ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()

glm(av_start_time ~ type + ld_b + beta_inc + beta_dec + alpha_init + alpha_inc + alpha_dec, data = out_data) %>% summary()
