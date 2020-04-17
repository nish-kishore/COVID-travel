#dependencies
source("./dependencies.R")

params <- read_yaml("driver.yaml")

with(params)

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


studypop_size <- sum(communities[,1])

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

results <- data.frame("Community"=rep(NA,studypop_size),"DayInfected"=rep(NA,studypop_size),"Simulation"=rep(NA,studypop_size),
                      "Symptoms"=rep(NA,studypop_size))




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
