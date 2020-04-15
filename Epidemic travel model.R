require(igraph)
require(Matrix)
require(Rlab)
require(ggplot2)
require(tidyverse)

# agents are in five compartments: SEI(A)IR (2 I compartments: asymptomatic, symptomatic)
# S are susceptible, E are infected but in the latency state, I are infected and infectious and R recovered.
# S+E+IAS+R move freely on a spatial network
# IS do not move

# position in list: S=1, E=2, IAS=3, IS=4, R=5

# Connectivity parameters
# make a list of all communities 
num_communities <- 3000
size_communities <- 1000
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

communities[c(25,65),"S"] <- 100000


studypop_size <- sum(communities[,1])
exp_grav <- 2 # exponent for gravity model

start_comm <- 25 # choose community where outbreak will start
num_inf <- 5 # choose number of initial infections

# Disease parameters (average 5 day latent and 10 day inf. period)
lat_per <- 0.2
rec_per <- 0.1

# Movement parameters
alpha_init<-0.001 #moving probability
beta_init<-0.15 # force of infection
perc_asymp<-0.5 # % asymptomatic

# Lockdown parameters
t_ld_a <- 1000 # change below in code once threshold cases reached
t_ld_b <- 1000 # change below in code once threshold cases reached
cases_ld_a <- 10 # number of cases when lockdown announced 
ld_b <- 10 # days after lockdownn announced that it begins
beta_inc <- 1.5 # relative beta after lockdown announced
beta_dec <- 0.8 # relative beta after lockdown begins (relative to initial beta)
alpha_inc <- 10 # relative travel after lockdown announced
alpha_dec <- 0.5 # relative travel decreases after lockdown begins (relative to initial travel)

num_timesteps <- 200
Nruns <- 1

# initial mobility network (gravity model)
mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
for (i in 1:num_communities){
  for (j in 1:num_communities){
    if (i!=j){
      mob_net[i,j] <- (sum(communities[[i]])*sum(communities[[i]]))/abs(i-j)^exp_grav
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
      mob_net2[i,j] <- abs(i-j)/(sum(communities[[i]])*sum(communities[[i]]))
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

results <- data.frame("Community"=rep(NA,studypop_size),"DayInfected"=rep(NA,studypop_size),"Simulation"=rep(NA,studypop_size))

for (irun in (1:Nruns)){
  # add infected people in community where starting; choose symptom status based on % asymp
  num_asymp <- sum(rbinom(num_inf,1,perc_asymp))
  communities[start_comm, "S"] <- communities[start_comm, "S"] - num_inf
  communities[start_comm, "A"]  <- num_asymp
  communities[start_comm, "I"] <- num_inf - num_asymp
  results[1:num_inf,] <- cbind(rep(start_comm,num_inf),rep(1,num_inf),rep(irun,num_inf))
  
  for (t in 1:num_timesteps){
    cat(irun,t,"\n")
    
    # if lockdown hasn't been announced yet check if it should be
    if (t_ld_a==1000){
      # should probably track symptomatic infections instead of setting threshold based on % asymp but leaving for now
      if (nrow(results[results$Community==start_comm & !is.na(results$Community),])>=cases_ld_a/(1-perc_asymp)){
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
      if (iloc==start_comm & t >= t_ld_a & t < t_ld_b){
          alpha <- alpha_init*alpha_inc
          beta< - beta_init*beta_inc 
      } else if (iloc==start_comm & t >= t_ld_b){
          alpha <- alpha_init*alpha_dec
          beta <- beta_init*beta_dec
      } else{
          alpha <- alpha_init
          beta <- beta_init
      }
      
      # recover
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
      
      if (num_new_inf >0){
        results[(num_inf+1):(num_inf+num_new_inf),] <- cbind(rep(iloc,num_new_inf),rep(t,num_new_inf),rep(irun,num_new_inf))
        num_inf <- num_inf + num_new_inf
      }
      
      # infect
      if (sum(communities[,"A"])>0 |
          sum(communities[,"I"])>0){
         beta_step <- beta/sum(communities[iloc,])
         tot_num_exp <- sum(communities[iloc,c(3,4)])
         N_exp <- sum(rbinom(communities[iloc, "S"],1,1-(1-beta_step)^tot_num_exp))
         communities[iloc, "E"] <- communities[iloc, "E"] + N_exp
         communities[iloc, "S"] <- communities[iloc, "S"] - N_exp
      }
      
      # move
      N_moveS <- sum(rbinom(sum(communities[iloc, "S"]),1,alpha))
      N_moveE <- sum(rbinom(sum(communities[iloc, "E"]),1,alpha))
      N_moveIAS <- sum(rbinom(sum(communities[iloc, "A"]),1,alpha))
      N_moveR <- sum(rbinom(sum(communities[iloc, "R"]),1,alpha))
      
      # remove number moving from community
      communities[iloc, "S"] <- communities[iloc, "S"] - N_moveS
      communities[iloc, "E"] <- communities[iloc, "E"] - N_moveE
      communities[iloc, "I"] <- communities[iloc, "I"] - N_moveIAS
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
  results %>% 
    mutate(t_ld_a=ifelse(t_ld_a==1000,NA,t_ld_a),
           t_ld_b=ifelse(t_ld_a==1000,NA,t_ld_b),
           travel_increase = alpha_init*alpha_inc,
           travel_decrease = alpha_init*alpha_dec,
           beta_increase = beta_init*beta_inc,
           beta_decrease = beta_init*beta_dec) -> results_master
}

#overall
results_master %>%
  filter(!is.na(Community)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary

#pre lockdown announcement summary + lag for inc period
results_master %>%
  filter(!is.na(Community) & DayInfected < (t_ld_a+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_prelockdown

results_master %>%
  filter(!is.na(Community) & DayInfected >=(t_ld_a+5) & DayInfected <= (t_ld_b+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_postlockdown_a

results_master %>%
  filter(!is.na(Community) & Simulation & DayInfected >=(t_ld_b+5)) %>%
  group_by(Community) %>%
  summarise(n=n()) -> community_summary_postlockdown_b

# add summary statistics for results_master -- time to X # infections, distribution across communities, etc

results_master %>%
  filter(!is.na(Community)) %>%
  group_by(DayInfected, Community, Simulation) %>%
  summarise(n=n()) %>%
  group_by(Community) %>%
  mutate(cumulative=cumsum(n)) -> community_day_summary

ggplot(community_day_summary, aes(x=DayInfected,y=cumulative,group=factor(Community),color=factor(Community))) + 
  geom_line() + theme_classic() +
  geom_vline(xintercept=t_ld_a) +
  geom_vline(xintercept=t_ld_b) +
  labs(color="Community",
       x="Days",
       y="Cumulative cases",
       caption = paste0("Starting community = ",start_comm,
                        "\n Total # cases = ", sum(community_day_summary$n),
                        "\n Vertical black lines indicate when lockdown was announced and began"))




