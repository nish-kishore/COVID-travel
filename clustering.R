library(igraph)
library(tidyverse)

network <- read_rds('./baseline_network.rds')

# Combined (sum) ---------

network %>% # starting with just summing everything together but can separate out 
  group_by(start_province,end_province) %>%
  subset(start_province!=end_province) %>%
  summarise(counts=sum(count)) %>%
  setNames(c("from","to","weights")) %>%
  spread(key=to,value=weights) %>%
  dplyr::select(-from) %>%
  as.matrix() %>%
  replace_na(0) -> network_total

g <- graph_from_adjacency_matrix(network_total,"directed",weighted=TRUE)

#plot.igraph(g) #plot
#is.weighted(g) # check weighted

clusters <- cluster_optimal(g,weights = E(g)$weights)
bind_cols("province"=clusters$names, "cluster"=clusters$membership) -> clusters_combined_sum

clusters_combined_sum %>%
  group_by(cluster) %>%
  summarise(n=n()) -> clusters_combined_summary_sum




# Combined (mean) ---------
# do it for all with average instead of summed
network %>% 
  group_by(start_province,end_province) %>%
  subset(start_province!=end_province) %>%
  summarise(counts=mean(count)) %>%
  setNames(c("from","to","weights")) %>%
  spread(key=to,value=weights) %>%
  dplyr::select(-from) %>%
  as.matrix() %>%
  replace_na(0) -> network_total

g <- graph_from_adjacency_matrix(network_total,"directed",weighted=TRUE)

#plot.igraph(g) #plot
#is.weighted(g) # check weighted

clusters <- cluster_optimal(g,weights = E(g)$weights)
bind_cols("province"=clusters$names, "cluster"=clusters$membership) -> clusters_combined_mean

clusters_combined_mean %>%
  group_by(cluster) %>%
  summarise(n=n()) -> clusters_combined_summary_mean



# Combined (by dow and tod) ---------

# do for each dow/tod combo
dows <- as.character(unique(network$dow))
tods <- as.character(unique(network$tod))

get_clusters <- function(network,d,t){
    network %>% 
      subset(dow==d & tod==t) %>%
      subset(start_province!=end_province) %>%
      dplyr::select(start_province,end_province,count) %>%
      setNames(c("from","to","weights")) %>%
      complete(from, to, fill = list(n = 0)) %>%
      spread(key=to,value=weights) %>%
      dplyr::select(-from) %>%
      as.matrix() %>%
      replace_na(0) -> network_dt
    
    g <- graph_from_adjacency_matrix(network_dt,"directed",weighted=TRUE)
    
    #plot.igraph(g) #plot
    is.weighted(g) # check weighted
    
    clusters <- cluster_optimal(g,weights = E(g)$weights)
    
    bind_cols("province"=clusters$names, "cluster"=clusters$membership) %>%
      mutate("dow"=d,
             "tod"=t) -> cluster_list
    
    return(cluster_list)
}

lapply(1:7,function(i){lapply(1:3,function(j){get_clusters(network,dows[i],tods[j])})}) %>%
  flatten() %>%
  bind_rows() -> cluster_list

cluster_list %>%
  group_by(cluster,dow,tod) %>%
  summarise(n=n()) -> cluster_summary

