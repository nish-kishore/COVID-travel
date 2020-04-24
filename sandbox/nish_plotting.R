source("../src/helper_functions.R")

results_log <- read_csv("./results_log.csv")
ids <- results_log[17:nrow(results_log), "unique_id"]

lapply(1:nrow(ids), function(x) load_merge_vars(results_log, ids[x,], alpha_init, ld_b, beta_inc, alpha_inc, beta_dec, alpha_dec)) %>%
  bind_rows() -> plot_data

plot_data %>%
  dplyr::select(Simulation, cumulative)