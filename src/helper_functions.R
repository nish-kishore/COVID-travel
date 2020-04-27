source("./src/dependencies.R")
source("./src/models.R")
source("./src/optim_funcs.R")

#specify rural/suburban/rural
get_comm_types <- function(num_communities){
  urban <- c(45,57)
  suburban <- c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
  rural <- setdiff(1:num_communities, c(urban, suburban))
  
  area_urban <- 4
  area_suburban <- 7
  area_rural <- 10
  
  n_urban <- 4000
  n_suburban <- 3500
  n_rural <- 1000
  
  return(list(urban, suburban, rural,
              area_urban, area_suburban, area_rural, 
              n_urban, n_suburban, n_rural))
}

#create the world objects for one set of parameters

init_model_objects <- function(params){
  with(params, {
    communities <- matrix(c(
      rep(size_communities, num_communities),
      rep(0, num_communities),
      rep(0, num_communities),
      rep(0, num_communities),
      rep(0, num_communities)
    ),
    nrow = num_communities
    )
    colnames(communities) <- c("S", "E", "A", "I", "R")

    com_types_list <- get_comm_types(num_communities)
    
    urban <- com_types_list[[1]]
    suburban <- com_types_list[[2]]
    rural <- com_types_list[[3]]

    area_urban <- com_types_list[[4]] # square miles?
    area_suburban <- com_types_list[[5]]
    area_rural <- com_types_list[[6]]

    communities[urban,"S"] <- com_types_list[[7]] # urban: makes it 1000 people/sq mile
    communities[suburban,"S"] <- com_types_list[[8]] # suburban 500 people/ sq mile
    communities[rural, "S"] <- com_types_list[[9]] #rural 100 people per square mile

    studypop_size <- sum(communities[,1])

    params$communities <- communities %>%
      as_tibble() %>%
      mutate(iloc = row_number(),
             comm_type = case_when(
               iloc %in% urban ~ "Urban",
               iloc %in% suburban ~ "Suburban",
               TRUE ~ "Rural"
             ),
             area = case_when(
               iloc %in% urban ~ 4,
               iloc %in% suburban ~ 7,
               TRUE ~ 10
             ),
             cum_symp = 0)
    params$studypop_size <- studypop_size


    num_edge <- sqrt(num_communities)
    row <- c(rep(1:num_edge,each=10))
    col <- c(rep(1:num_edge,times=10))
    # initial mobility network (gravity model)
    mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        if (i!=j){
          mob_net[i,j] <- (sum(communities[[i]])*sum(communities[[j]]))/
                                      (abs(row[i]-row[j]) + abs(col[i] - col[j]))^exp_grav
        }
      }
    }



    mob_net_norm <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        mob_net_norm[i,j] <- mob_net[i,j]/sum(mob_net[i,])
      }
    }

    # mobility network post lockdown announcement - increase probabilities further away
    mob_net2 <- mob_net_norm
    mob_net2[,start_comm] <- 0 # prevent any travel to urban cities
    #mob_net2[,suburban] <- 0 # prevent travel to suburban areas

    mob_net_norm2 <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        mob_net_norm2[i,j] <- mob_net2[i,j]/sum(mob_net2[i,])
      }
    }
    params$mob_net_norm <- mob_net_norm
    params$mob_net_norm2 <- mob_net_norm2
    params$results <- vector(mode = "list", length = num_timesteps)

    params$urban <- urban
    params$suburban <- suburban
    params$rural <- rural

    params$area_urban <- area_urban
    params$area_suburban <- area_suburban
    params$area_rural <- area_rural

    return(params)
  })
}

#pack and run all model objects
pack_and_run_models <- function(params){
  packed_model_objects <- init_model_objects(params)

  replicate(params$Nruns, model_a_optim(packed_model_objects), simplify = FALSE) %>%
    bind_rows(.id = "Simulation") %>%
    write_rds(paste0("./cache/results/",params$unique_id,".rds"))

  return(as_tibble(cbind("date_time" = Sys.time(), "user" = as.character(Sys.info()["login"]), as_tibble(params))))

}

#takes in parameters from the driver file and runs the model
run_models <- function(driver_file_path, cores = NULL){

  #reads and expands grid of all possible values
  params_df <- read_yaml(driver_file_path) %>% expand.grid() %>% as_tibble()

  #creates unique id hash
  params_df$unique_id <- apply(params_df, 1, digest)

  #load in results log
  suppressMessages(
    results_log <- read_csv("./results_log.csv")
  )

  #subset to params that we don't already have a result for
  new_params_df <- subset(params_df, !unique_id %in% results_log$unique_id)
  done_params_df <- subset(params_df, unique_id %in% results_log$unique_id)

  print(paste("There are", nrow(new_params_df), "new parameter combinations to run.", nrow(done_params_df),
              "combinations have already been run previously and will be skipped."))

  list_of_params <- transpose(new_params_df)

  #set up parallel processing
  if(is.null(cores)){
    cl <- makeCluster(detectCores()-2)
  }else{
    cl <- makeCluster(cores)
  }
  registerDoParallel(cl)
  print(paste0("Starting cluster ", nrow(new_params_df), " jobs identified."))

  foreach(i = 1:nrow(new_params_df),
          .export = c("model_a_optim", "run_models", "init_model_objects", "pack_and_run_models",
                      "update_disease_status", "update_loc", "update_mob_data", "get_comm_types"),
          .packages = c("tidyverse"),
          .combine = rbind) %dopar% {pack_and_run_models(list_of_params[[i]])} -> out_results

  results_log <- rbind(results_log, out_results)

  write_csv(results_log, "./results_log.csv")

  #stop parallel processing
  stopCluster(cl)

}

#reads in results from a given run
load_run_results <- function(unique_id){
  return(read_rds(paste0("./cache/results/",unique_id,".rds")))
}
