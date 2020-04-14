require(igraph)
require(Matrix)
require(Rlab)
require(ggplot2)
require(tidyverse)

# agents are in five compartments: SEI(A)IR (2 I compartments: asymptomatic, symptomatic)
# S are susceptible, E are infected but in the latency state, I are infected and infectious and R recovered.
# S+E+IAS+R move freely on a spatial network
# IS do not move

# states: S=1, E=2, IAS=3, IS=4, R=5

# Connectivity parameters
community_sizes <- rep(100,100) 
community_sizes[c(45,20)] <- 1000 # for now make mostly smaller and 2 big "cities"; can scale up later
studypop_size <- sum(community_sizes)
num_communities <- length(community_sizes)
rate_within <- 1 # connectivity w/in a communiity - for now make well mixed but can cluster later
rate_between <- 0 # keep separate so only transmit to another community if moved there
exp_grav <- 2 # exponent for gravity model

start_comm <- 45 # choose community where outbreak will start
num_inf <- 5 # choose number of initial infections

# Disease parameters (need to update but for now gamma distribution 5 day latent and 7 day inf. period)
latperiod_shape<-5
latperiod_rate<-0.9
infperiod_shape<-1.13
infperiod_rate<-0.16
perc_asymp <- 0.5 

# Movement parameters
alpha_init<-0.1 #moving probability
beta_init<-0.15 # force of infection
perc_asymp<-0.5 # % asymptomatic

# Lockdown parameters
t_ld_a <- 20 #time lockdown announced - I think we should probably link this to # infections instead of being fixed
t_ld_b <- 30 #time lockdown begins
beta_inc <- 1.5 # amount beta increases after lockdown announced
beta_dec <- 0.5 # amount beta decreases after lockdown begins (relative to initial beta)
alpha_inc <- 2 # amount travel increases after lockdown announced
alpha_dec <- 0.3 # amount travel decreases after lockdown begins (relative to initial travel)

num_timesteps <- 200
Nruns <- 1

# initial mobility network (gravity model)
mob_net <- matrix(0,nrow=num_communities,ncol=num_communities)
for (i in 1:num_communities){
  for (j in 1:num_communities){
    if (i!=j){
      mob_net[i,j] <- (community_sizes[i]*community_sizes[j])/abs(i-j)^exp_grav
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
      mob_net2[i,j] <- abs(i-j)/(community_sizes[i]*community_sizes[j])
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

# Function to make network
make_network <- function(community_sizes,num_communities,studypop_size,rate_within, rate_between) {
  within_rates <- diag(nrow=num_communities,ncol=num_communities,x=rate_within)
  between_rates <- matrix(rate_between,nrow=num_communities,ncol=num_communities) -
    diag(nrow=num_communities,ncol=num_communities,x=rate_between)
  rates<-within_rates+between_rates
  
  g <- sample_sbm(studypop_size,rates,community_sizes)

  V(g)$name<-1:sum(community_sizes) # Give the nodes a name so that igraph remembers them
  V(g)$community<-rep(1:num_communities,community_sizes)
  V(g)$state <- 1 # make everyone susceptible initially
  V(g)$inf_time <- 1000 # track infection times
  V(g)$rec_time <- 1000 # track recover times
  
  return(g)
}

g <- make_network(community_sizes,num_communities,studypop_size,rate_within, rate_between)

results <- data.frame("InfectedNode"=rep(NA,studypop_size),"Community"=rep(NA,studypop_size),"DayInfected"=rep(NA,studypop_size),"Symptoms"=rep(NA,studypop_size))

for (irun in (1:Nruns)){
  # add infected people in community where starting; choose symptom status based on % asymp
  init_inf <- sample(V(g)[community==start_comm]$name,num_inf)
  num_asymp <- sum(rbinom(num_inf,1,perc_asymp))
  V(g)[init_inf]$state <- c(rep(3,num_asymp),rep(4,num_inf-num_asymp))
  V(g)[init_inf]$rec_time <- 1 + round(rgamma(length(init_inf),infperiod_shape,infperiod_rate))
  results[1:num_inf,1:4] <- cbind(init_inf,rep(start_comm,num_inf),rep(1,num_inf),V(g)[init_inf]$state)
  
  for (t in 1:num_timesteps){
    cat(irun,t,"\n")
    if (t >=t_ld_b){
      mob_net <- mob_net_norm2
    } else if (t>=t_ld_a & t <t_ld_b){
      mob_net <- mob_net_norm2 # for now leaving same but could have a third one if want
    } else{
      mob_net <- mob_net_norm
    }
    # move Is --> R and E --> Is
    # recover
    V(g)[rec_time==t]$state <- 5
    # exposed --> infected
    N_move_I <- length(V(g)[inf_time==t])
    if (N_move_I > 0){
      V(g)[inf_time==t]$state <- ifelse(rbinom(N_move_I,1,perc_asymp)==1,3,4)
      results[(num_inf+1):(num_inf+N_move_I),] <- cbind(V(g)[inf_time==t]$name,V(g)[inf_time==t]$community,rep(t,N_move_I), V(g)[inf_time==t]$state)
      num_inf <- num_inf + N_move_I
    }
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
      # first move
      N_move <- sum(rbinom(sum(V(g)$community==iloc)-sum(V(g)[community==iloc]$state==4), 1,alpha))
      agents_move <- sample(V(g)[state %in% c(1,2,3,5) & community == iloc]$name,N_move,replace=FALSE)
      if (length(agents_move)>0){
        dest <- sample(c(1:num_communities),N_move,mob_net[iloc,],replace=TRUE)
        for (a in 1:length(agents_move)){
          V(g)[agents_move[a]]$community <- dest[a] # update community
        }
      }
      # infect
      if (sum(V(g)[community==iloc]$state %in% c(3,4))){
         beta_step <- beta/length(V(g)[community==iloc])
         tot_num_exp <- sum(V(g)[community==iloc]$state %in% c(3,4))
         N_exp <- sum(rbinom(sum(V(g)[community==iloc]$state==1),1,1-(1-beta_step)^tot_num_exp))
         new_exp <- sample(V(g)[community==iloc & state==1],N_exp,replace=FALSE)
         V(g)[new_exp]$state <- 2
         V(g)[new_exp]$inf_time <- t + round(rgamma(length(new_exp),latperiod_shape,latperiod_rate))
         V(g)[new_exp]$rec_time <- V(g)[new_exp]$inf_time + round(rgamma(length(new_exp),infperiod_shape,infperiod_rate))
       }
    }
  }
}


# add summary statistics for results -- time to X # infections, distribution across communities, etc




