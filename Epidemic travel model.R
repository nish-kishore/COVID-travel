#dependencies
source("./src/dependencies.R")
source("./src/models.R")
source("./src/helper_functions.R")
#source("./src/generate_plots.R")


#run model using inputs from driver file
run_models(driver_file_path = "./drivers/driver_v2.yaml", cores = 14, force_run = F)

#generate heatmaps
#generate_plots(run_all = T)
