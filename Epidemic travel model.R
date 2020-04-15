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
num_communities <- 100
size_communities <- 1000
communities <- vector("list",num_communities)
communities <- lapply(communities,function(x)x[1:5]=c(size_communities,0,0,0,0)) # start everyone out susceptible
communities[[25]][1] <- 10000 # make city
communities[[65]][1] <- 10000 # make city


studypop_size <- sum(unlist(communities))
exp_grav <- 2 # exponent for gravity model

start_comm <- 25 # choose community where outbreak will start
num_inf <- 5 # choose number of initial infections

# Disease parameters (average 5 day latent and 10 day inf. period)
lat_per <- 0.2
rec_per <- 0.1

# Movement parameters
alpha_init<-0.01 #moving probability
beta_init<-0.15 # force of infection
perc_asymp<-0.5 # % asymptomatic

# Lockdown parameters
t_ld_a <- 1000 # change below in code once threshold cases reached
t_ld_b <- 1000 # change below in code once threshold cases reached
cases_ld_a <- 20 # number of cases when lockdown announced 
ld_b <- 10 # days after lockdownn announced that it begins
beta_inc <- 1.5 # relative beta after lockdown announced
beta_dec <- 0.8 # relative beta after lockdown begins (relative to initial beta)
alpha_inc <- 1 # relative travel after lockdown announced
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
  communities[[start_comm]][[1]] <- communities[[start_comm]][[1]] - num_inf
  communities[[start_comm]][[3]]  <- num_asymp
  communities[[start_comm]][[4]] <- num_inf - num_asymp
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
      recover_AS <- sum(rbinom(communities[[iloc]][3],1,rec_per))
      recover_S <- sum(rbinom(communities[[iloc]][4],1,rec_per))
      communities[[iloc]][3] <- communities[[iloc]][3] - recover_AS
      communities[[iloc]][4] <- communities[[iloc]][4] - recover_S
      communities[[iloc]][5] <- communities[[iloc]][5] + recover_AS + recover_S
      
      # exposed --> infected
      num_new_inf <- sum(rbinom(communities[[iloc]][2],1,lat_per))
      N_asymp <- sum(rbinom(num_new_inf,1,perc_asymp))
      communities[[iloc]][2] <- communities[[iloc]][2] - num_new_inf
      communities[[iloc]][3] <- communities[[iloc]][3] + N_asymp
      communities[[iloc]][4] <- communities[[iloc]][4] + (num_new_inf - N_asymp)
      
      if (num_new_inf >0){
        results[(num_inf+1):(num_inf+num_new_inf),] <- cbind(rep(iloc,num_new_inf),rep(t,num_new_inf),rep(irun,num_new_inf))
        num_inf <- num_inf + num_new_inf
      }
      
      # infect
      if (sum(unlist(lapply(communities,function(x)(x[3]))))>0 |
          sum(unlist(lapply(communities,function(x)(x[4]))))>0){
         beta_step <- beta/sum(communities[[iloc]])
         tot_num_exp <- sum(communities[[iloc]][c(3,4)])
         N_exp <- sum(rbinom(communities[[iloc]][1],1,1-(1-beta_step)^tot_num_exp))
         communities[[iloc]][2] <- communities[[iloc]][2] + N_exp
         communities[[iloc]][1] <- communities[[iloc]][1] - N_exp
      }
      
      # move
      N_moveS <- sum(rbinom(sum(communities[[iloc]][1]),1,alpha))
      N_moveE <- sum(rbinom(sum(communities[[iloc]][2]),1,alpha))
      N_moveIAS <- sum(rbinom(sum(communities[[iloc]][3]),1,alpha))
      N_moveR <- sum(rbinom(sum(communities[[iloc]][5]),1,alpha))
      
      # remove number moving from community
      communities[[iloc]][1] <- communities[[iloc]][1] - N_moveS
      communities[[iloc]][2] <- communities[[iloc]][2] - N_moveE
      communities[[iloc]][3] <- communities[[iloc]][3] - N_moveIAS
      communities[[iloc]][5] <- communities[[iloc]][5] - N_moveR
      
      # add those moving to new communities
      if (sum(N_moveS+N_moveE+N_moveIAS+N_moveR)>0){
        destS <- sample(c(1:num_communities),N_moveS,mob_net[iloc,],replace=TRUE)
        destE <- sample(c(1:num_communities),N_moveE,mob_net[iloc,],replace=TRUE)
        destIAS <- sample(c(1:num_communities),N_moveIAS,mob_net[iloc,],replace=TRUE)
        destR <- sample(c(1:num_communities),N_moveR,mob_net[iloc,],replace=TRUE)
        
        for (dest in destS){
          communities[[dest]][1] <-   communities[[dest]][1] +1
        }
        
        for (dest in destE){
          communities[[dest]][2] <-   communities[[dest]][2] +1
        }
        
        for (dest in destIAS){
          communities[[dest]][3] <-   communities[[dest]][3] +1
        }
        
        for (dest in destR){
          communities[[dest]][5] <-   communities[[dest]][5] +1
        }
        
      }
    }
  }
}

#overall
results %>%
  filter(!is.na(Community)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary

#pre lockdown announcement summary + lag for inc period
results %>%
  filter(!is.na(Community) & DayInfected < (t_ld_a+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_prelockdown

results %>%
  filter(!is.na(Community) & DayInfected >=(t_ld_a+5) & DayInfected <= (t_ld_b+5)) %>%
  group_by(Community, Simulation) %>%
  summarise(n=n()) -> community_summary_postlockdown_a

results %>%
  filter(!is.na(Community) & Simulation & DayInfected >=(t_ld_b+5)) %>%
  group_by(Community) %>%
  summarise(n=n()) -> community_summary_postlockdown_b

# add summary statistics for results -- time to X # infections, distribution across communities, etc





