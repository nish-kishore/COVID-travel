source("./src/dependencies.R")
source("./src/models.R")

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
    
    communities[c(25,65),"S"] <- 1000
    urban <- c(45,57)
    suburban <-c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
    rural <- setdiff(1:num_communities, c(urban, suburban))
    
    area_urban <- 500 # square miles?
    area_suburban <- 700 
    area_rural <- 1000
    
    communities[urban,"S"] <- 10000 # urban 
    communities[suburban,"S"] <- 5000 # suburban
    
    studypop_size <- sum(communities[,1])
    
    params$communities <- communities
    params$studypop_size <- studypop_size
    
    # initial mobility network (gravity model)
    mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        if (i!=j){
          mob_net[i,j] <- (sum(communities[[i]])*sum(communities[[j]]))/abs(i-j)^exp_grav
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
    mob_net2 <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        if (i!=j){
          mob_net2[i,j] <- abs(i-j)/(sum(communities[[i]])*sum(communities[[j]]))
        }
      }
    }
    mob_net2[,start_comm] <- 0 # prevent travel into city on lockdown
    mob_net_norm2 <- matrix(0,nrow=num_communities,ncol=num_communities)
    for (i in 1:num_communities){
      for (j in 1:num_communities){
        mob_net_norm2[i,j] <- mob_net2[i,j]/sum(mob_net2[i,])
        # right now I just reversed gravity model so further away / smaller is higher but need to refine
      }
    }
    
    params$mob_net_norm <- mob_net_norm 
    params$mob_net_norm2 <- mob_net_norm2
    params$results <- data.frame("Community"=rep(NA,studypop_size),"DayInfected"=rep(NA,studypop_size),"Simulation"=rep(NA,studypop_size),
                                 "Symptoms"=rep(NA,studypop_size))
    
    params$urban <- urban
    params$suburban <- suburban
    params$rural <- rural
    
    params$area_urban <- area_urban
    params$area_suburban <- area_suburban 
    params$area_rural <- area_rural
    
    return(params)
  })
}


#takes in parameters from the driver file and runs the model
run_model <- function(driver_file_path){
  params_df <- read_yaml(driver_file_path) %>% as_tibble()
  params_df$unique_id <- apply(params_df, 1, digest)
  
  packed_model_objects <- lapply(1:nrow(params_df), function(x) init_model_objects(as.list(params_df[x,])))
  suppressMessages(
    results_log <- read_csv("./results_log.csv")
  )

  
  #set up parallel processing
  cl <- makeCluster(detectCores()-2)
  registerDoParallel(cl)
  cat(paste0("Starting cluster ", length(packed_model_objects), " jobs identified."))
  
  for(i in 1:length(packed_model_objects)){
    if(!params_df[i,"unique_id"] %in% results_log$unique_id){
      params <- packed_model_objects[[i]]
      
      foreach(irun = 1:params$Nruns, .export = "model_a", .combine = rbind) %dopar% {model_a(params, irun)} %>%
        write_csv(paste0("./cache/results/",params_df[i,"unique_id"],".csv"))
      
      cat(paste0("Job ", i, "/", length(packed_model_objects), " - Completed"))
      results_log <- rbind(results_log,cbind("date_time" = Sys.time(), "user" = as.character(Sys.info()["login"]), params_df[i,]))
    }
    
  }
  write_csv(results_log, "./results_log.csv")
  
  #stop parallel processing
  stopCluster(cl)
  
}

#reads in results from a given run
load_run_results <- function(unique_id){
  return(read_csv("cache/results/",unique_id,".csv"))
}