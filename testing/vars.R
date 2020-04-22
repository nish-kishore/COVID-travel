source("./src/helper_functions.R")
source("./testing/optim_funcs.R")

#base variales
params_df <- read_yaml("driver.yaml") %>% expand.grid() %>% as_tibble()

#creates unique id hash
params_df$unique_id <- apply(params_df, 1, digest)

params <- init_model_objects(transpose(params_df[1,])[[1]])

list2env(params, .GlobalEnv)

