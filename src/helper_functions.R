source("./src/dependencies.R")
source("./src/models.R")
source("./src/optim_funcs.R")

#specify rural/suburban/rural

get_comm_types <- function(num_communities, comm_version){
  if(comm_version == 1){
    urban <- c(45,57)
    suburban <- c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
    rural <- setdiff(1:num_communities, c(urban, suburban))

    area_urban <- 4
    area_suburban <- 7
    area_rural <- 10

    n_urban <- 4000
    n_suburban <- 3500
    n_rural <- 1000
  }

  if(comm_version == 2){
    urban <- c(45,57)
    suburban <- c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
    rural <- setdiff(1:num_communities, c(urban, suburban))

    area_urban <- 4
    area_suburban <- 10
    area_rural <- 10

    n_urban <- 4000
    n_suburban <- 2500
    n_rural <- 2500
  }

  if(comm_version == 3){
    urban <- c(45)
    suburban <- c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
    rural <- setdiff(1:num_communities, c(urban, suburban))

    area_urban <- 4
    area_suburban <- 10
    area_rural <- 10

    n_urban <- 4000
    n_suburban <- 2500
    n_rural <- 2500
  }

  if(comm_version == 4){
    urban <- c(45)
    suburban <- c(15,22,29,45,72,79,85)
    rural <- setdiff(1:num_communities, c(urban, suburban))

    area_urban <- 4
    area_suburban <- 7
    area_rural <- 10

    n_urban <- 4000
    n_suburban <- 3500
    n_rural <- 1000

    cluster1 <- c(1,2,3,11,12,13,21,22,23,31,32,33,41,42,43)
    cluster2 <- c(4,5,6,14,15,16,24,25,26)
    cluster3 <- c(7,8,9,10,17,18,19,20,27,28,29,30,37,38,39,40,47,48,49,50)
    cluster4 <- c(51,52,53,61,62,63,71,72,73,81,82,83,91,92,93)
    cluster5 <- c(34,35,36,44,45,46,54,55,56,64,65,66)
    cluster6 <- c(74,75,76,84,85,86,94,95,96)
    cluster7 <- c(57,58,59,60,67,68,69,70,77,78,79,80,87,88,89,90,97,98,99,100)

    bind_rows (
      bind_cols("cluster"=rep(1,length(cluster1)),"community"=cluster1),
      bind_cols("cluster"=rep(2,length(cluster2)),"community"=cluster2),
      bind_cols("cluster"=rep(3,length(cluster3)),"community"=cluster3),
      bind_cols("cluster"=rep(4,length(cluster4)),"community"=cluster4),
      bind_cols("cluster"=rep(5,length(cluster5)),"community"=cluster5),
      bind_cols("cluster"=rep(6,length(cluster6)),"community"=cluster6),
      bind_cols("cluster"=rep(7,length(cluster7)),"community"=cluster7)) -> clusters

    }

  if(comm_version == 5){ # if we want to update based on Spain data
    urban <- c(66)
    suburban <- c(23:27,33:39,43:44,46:49,53:56,58:59,63:65,67:69,76:79)
    rural <- setdiff(1:num_communities, c(urban, suburban))

    area_urban <- 4
    area_suburban <- 10
    area_rural <- 10

    n_urban <- 4000
    n_suburban <- 2500
    n_rural <- 2500
  }

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

    com_types_list <- get_comm_types(num_communities, comm_version)

    urban <- com_types_list[[1]]
    suburban <- com_types_list[[2]]
    rural <- com_types_list[[3]]

    area_urban <- com_types_list[[4]] # square miles?
    area_suburban <- com_types_list[[5]]
    area_rural <- com_types_list[[6]]

    communities[urban,"S"] <- com_types_list[[7]] # urban: makes it 1000 people/sq mile
    communities[suburban,"S"] <- com_types_list[[8]] # suburban 500 people/ sq mile
    communities[rural, "S"] <- com_types_list[[9]] #rural 100 people per square mile

    #clusters <- com_types_list[[10]] #get clusters

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
               iloc %in% urban ~ area_urban,
               iloc %in% suburban ~ area_suburban,
               TRUE ~ area_rural
             ),
             cum_symp = 0)
    params$studypop_size <- studypop_size


    num_edge <- sqrt(num_communities)
    row <- c(rep(1:num_edge,each=10))
    col <- c(rep(1:num_edge,times=10))
    # initial mobility network (gravity model)
    if(comm_version == 4){
      mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
      for (i in 1:num_communities){
        for (j in 1:num_communities){
          if (i!=j){
            cluster.i <- clusters %>% subset(community==i) %>% pull(cluster)
            cluster.j <- clusters %>% subset(community==j) %>% pull(cluster)
            if (cluster.i==cluster.j){
              clust <- clust_mult
            } else{
              clust <- 1
            }
            mob_net[i,j] <- clust*((sum(communities[[i]])^a0)*(sum(communities[[j]])^b0))/
              (abs(row[i]-row[j]) + abs(col[i] - col[j]))^exp_grav0
          }
        }
      }
    } else if(comm_version == 5){
      mob_net_norm <- read_rds(paste0("./resources/mob_net/",params$mob_net_name,".rds"))
      # load list - assume normalized, but if not add code to normalize
      mob_net_norm2 <- NA
    } else{
      mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
      for (i in 1:num_communities){
        for (j in 1:num_communities){
          if (i!=j){
            mob_net[i,j] <- ((sum(communities[[i]])^a0)*(sum(communities[[j]])^b0))/
              (abs(row[i]-row[j]) + abs(col[i] - col[j]))^exp_grav0
          }
        }
      }
    }

    if (comm_version!=5){ # already normalized
      mob_net_norm <- matrix(0,nrow=num_communities,ncol=num_communities)
      for (i in 1:num_communities){
        for (j in 1:num_communities){
          mob_net_norm[i,j] <- mob_net[i,j]/sum(mob_net[i,])
        }
      }
      mob_net_norm2 <- mob_net_norm
    }
    # # mobility network post lockdown announcement - increase probabilities further away
    # mob_net2 <- matrix(0,nrow=num_communities,ncol=num_communities)
    # for (i in 1:num_communities){
    #   for (j in 1:num_communities){
    #     if (i!=j){
    #       mob_net2[i,j] <- ((sum(communities[[i]])^a1)*(sum(communities[[j]])^b1))/
    #         (abs(row[i]-row[j]) + abs(col[i] - col[j]))^exp_grav1
    #     }
    #   }
    # }
    #
    #
    # mob_net2[,start_comm] <- 0 # prevent any travel to urban cities
    # #mob_net2[,suburban] <- 0 # prevent travel to suburban areas
    #
    # mob_net_norm2 <- matrix(0,nrow=num_communities,ncol=num_communities)
    # for (i in 1:num_communities){
    #   for (j in 1:num_communities){
    #     mob_net_norm2[i,j] <- mob_net2[i,j]/sum(mob_net2[i,])
    #   }
    # }
    params$mob_net_norm <- mob_net_norm
    params$mob_net_norm2 <- mob_net_norm2
    params$results <- vector(mode = "list", length = num_timesteps)

    # list of mob_nets
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
run_models <- function(driver_file_path, cores = NULL, force_run = F){

  #reads and expands grid of all possible values
  params_df <- read_yaml(driver_file_path) %>% 
    expand.grid() %>% 
    as_tibble() %>% 
    mutate(mob_net_name = as.character(mob_net_name))

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
              "combinations have already been run previously and will be skipped unless you've selected to rerun."))

  if(force_run){
    runs_params_df <- params_df
  }else{
    runs_params_df <- new_params_df
  }

  list_of_params <- transpose(runs_params_df)

  #set up parallel processing
  if(is.null(cores)){
    cl <- makeCluster(detectCores()-2)
  }else{
    cl <- makeCluster(cores)
  }
  registerDoParallel(cl)
  print(paste0("Starting cluster ", nrow(runs_params_df), " jobs identified."))

  foreach(i = 1:nrow(runs_params_df),
          .export = c("model_a_optim", "run_models", "init_model_objects", "pack_and_run_models",
                      "update_disease_status", "update_loc", "update_mob_data", "get_comm_types"),
          .packages = c("tidyverse"),
          .combine = rbind) %dopar% {pack_and_run_models(list_of_params[[i]])} -> out_results

  results_log <- bind_rows(results_log, out_results)

  write_csv(results_log, "./results_log.csv")

  #stop parallel processing
  stopCluster(cl)

}

#reads in results from a given run
load_run_results <- function(unique_id){
  return(read_rds(paste0("./cache/results/",unique_id,".rds")) %>%
           mutate(id = unique_id))
}

#attached vars from results into data
load_merge_vars <- function(results_log,id,...){
  load_run_results(id) %>%
    cbind(subset(results_log, unique_id %in% id) %>% dplyr::select(...)) %>%
    as_tibble()
}
