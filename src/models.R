#Model A

model_a <- function(params){
  require(tidyverse)
    with(params, {
      irun <- NA
      # add infected people in community where starting; choose symptom status based on % asymp
      num_asymp <- sum(rbinom(num_inf,1,perc_asymp))
      communities[start_comm, "S"] <- communities[start_comm, "S"] - num_inf
      communities[start_comm, "A"]  <- num_asymp
      communities[start_comm, "I"] <- num_inf - num_asymp
      results[1:num_inf,] <- cbind(rep(start_comm,num_inf),rep(1,num_inf),rep(irun,num_inf),c(rep(1,(num_inf - num_asymp)),rep(0,num_asymp)))
      
      for (t in 1:num_timesteps){
        #cat(irun,t,"\n")
        
        # if lockdown hasn't been announced yet check if it should be
        if (t_ld_a==1000){
          # should probably track symptomatic infections instead of setting threshold based on % asymp but leaving for now
          if (nrow(results[results$Community==start_comm & !is.na(results$Community) &
                           results$Symptoms==1,])>=cases_ld_a){
            t_ld_a <- t
            t_ld_b <- t + ld_b
          }
          mob_net <- mob_net_norm
        } else { 
          if (t >=t_ld_b){
            mob_net <- mob_net_norm2
          } else if (t>=t_ld_a & t <t_ld_b){
            mob_net <- mob_net_norm2 # for now leaving same but could have a third one if want
          } 
        }
        # move Is --> R and E --> Is
        # recover
        
        for (iloc in 1:num_communities){
          if (iloc==start_comm & t >= t_ld_a & t < t_ld_b){ # change alpha and beta if lockdown has been announced in starting comm
            alpha <- alpha_init*alpha_inc
            beta <- beta_init*beta_inc 
          } else if (iloc==start_comm & t >= t_ld_b){ # change alpha and beta if lockdown has begun in starting comm
            alpha <- alpha_init*alpha_dec
            beta <- beta_init*beta_dec
          } else if (nrow(results[results$Community==iloc & !is.na(results$Community) &
                                    results$Symptoms==1,])>=cases_ld_a){ 
            # change beta if threshold reached in other communnities but keep alpha the same
            beta <- beta_init*beta_dec
          } else {
            alpha <- alpha_init
            beta <- beta_init
          }
          
        
        
          
          # recover
          #cat(communities)
          recover_AS <- sum(rbinom(communities[iloc, "A"],1,rec_per))
          recover_S <- sum(rbinom(communities[iloc, "I"],1,rec_per))
          communities[iloc, "A"] <- communities[iloc, "A"] - recover_AS
          communities[iloc, "I"] <- communities[iloc, "I"] - recover_S
          communities[iloc, "R"] <- communities[iloc, "R"] + recover_AS + recover_S
          
          # exposed --> infected
          num_new_inf <- sum(rbinom(communities[iloc, "E"],1,lat_per))
          N_asymp <- sum(rbinom(num_new_inf,1,perc_asymp))
          communities[iloc, "E"] <- communities[iloc, "E"] - num_new_inf
          communities[iloc, "A"] <- communities[iloc, "A"] + N_asymp
          communities[iloc, "I"] <- communities[iloc, "I"] + (num_new_inf - N_asymp)
          
          if (num_new_inf > 0){
            results[(num_inf+1):(num_inf+num_new_inf),] <- cbind(rep(iloc,num_new_inf),rep(t,num_new_inf),rep(irun,num_new_inf),
                                                                 c(rep(1,(num_new_inf - N_asymp)),rep(0,N_asymp)))
            num_inf <- num_inf + num_new_inf
          }
          
          # infect
          if (sum(communities[,"A"])>0 |
              sum(communities[,"I"])>0){
            area <- ifelse(iloc %in% urban, area_urban, (ifelse( iloc %in% suburban, area_suburban,area_rural)))
            beta_step <- beta/area
            tot_num_exp <- sum(communities[iloc,c(3,4)])
            N_exp <- sum(rbinom(communities[iloc, "S"],1,1-(1-beta_step)^tot_num_exp))
            communities[iloc, "E"] <- communities[iloc, "E"] + N_exp
            communities[iloc, "S"] <- communities[iloc, "S"] - N_exp
          }
          
          # move
          N_moveS <- sum(rbinom(sum(communities[iloc, "S"]),1,alpha))
          N_moveE <- sum(rbinom(sum(communities[iloc, "E"]),1,alpha))
          N_moveIAS <-sum(rbinom(sum(communities[iloc, "A"]),1,alpha))
          N_moveR <- sum(rbinom(sum(communities[iloc, "R"]),1,alpha))
          
          # remove number moving from community
          communities[iloc, "S"] <- communities[iloc, "S"] - N_moveS
          communities[iloc, "E"] <- communities[iloc, "E"] - N_moveE
          communities[iloc, "A"] <- communities[iloc, "A"] - N_moveIAS
          communities[iloc, "R"] <- communities[iloc, "R"] - N_moveR
          
          # add those moving to new communities
          if (sum(N_moveS+N_moveE+N_moveIAS+N_moveR)>0){
            destS <- sample(c(1:num_communities),N_moveS,mob_net[iloc,],replace=TRUE)
            destE <- sample(c(1:num_communities),N_moveE,mob_net[iloc,],replace=TRUE)
            destIAS <- sample(c(1:num_communities),N_moveIAS,mob_net[iloc,],replace=TRUE)
            destR <- sample(c(1:num_communities),N_moveR,mob_net[iloc,],replace=TRUE)
            
            for (dest in destS){
              communities[dest,"S"] <-   communities[dest,"S"] +1
            }
            
            for (dest in destE){
              communities[dest,"E"] <-   communities[dest,"E"] +1
            }
            
            for (dest in destIAS){
              communities[dest,"A"] <-   communities[dest,"A"] +1
            }
            
            for (dest in destR){
              communities[dest,"R"] <-   communities[dest,"R"] +1
            }
            
          }
        }
      }
      results$t_ld_a <- ifelse(t_ld_a==1000,NA,t_ld_a)
      results$t_ld_b <- ifelse(t_ld_a==1000,NA,t_ld_b)
      results$travel_increase <- alpha_init*alpha_inc
      results$travel_decrease <- alpha_init*alpha_dec
      results$beta_increase <- beta_init*beta_inc 
      results$beta_decrease <- beta_init*beta_dec
      results$type <- ifelse(results$Community %in% urban, "U",
                             ifelse(results$Community %in% suburban, "S", "R"))
      
      results %>%
        filter(!is.na(Community)) %>%
        group_by(DayInfected, Simulation, Community, type, t_ld_a) %>%
        summarise(n=n()) %>%
        group_by(Simulation, Community) %>%
        mutate(cumulative=cumsum(n)) -> results_summary
      
      return(results_summary)
      
    })

}
