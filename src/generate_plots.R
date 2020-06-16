#scripts to generate plots
source("./src/helper_functions.R")
library(scales)

create_heatmap <- function(rds_id,comm_version,threshold,day){
  
  suppressMessages(
    params <- read_csv("./results_log.csv") %>% subset(unique_id == rds_id)
  )
  
  data <- read_rds(paste0("./cache/results/",rds_id,".rds"))
  
  data %>%
    group_by(Simulation,DayInfected, Community, type, t_ld_a) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Community) %>%
    mutate(cumulative=cumsum(n)) %>%
    ungroup() -> data1
  
  comm_type_list <- get_comm_types(params1$num_communities,comm_version)
  
  city <- comm_type_list[[1]]
  noncity <- c(comm_type_list[[2]],comm_type_list[[3]])
  
  data1$type <- ifelse(data1$Community %in% city, "City","Non-city")

  num_edge <- sqrt(params1$num_communities)
  row <- c(rep(1:num_edge,each=10))
  col <- c(rep(1:num_edge,times=10))
  
  # summarise total # infections and start times by community
  
  data1 %>%
    group_by(Simulation,DayInfected, Community, type, t_ld_a) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Community) %>%
    mutate(cumulative=cumsum(n)) %>%
    ungroup() -> data1
  
  data1 %>%
    filter(DayInfected<=day) %>%
    group_by(Community,Simulation, type) %>%
    summarise(infections = sum(n),
              start = min(DayInfected),
              t_ld_a = min(t_ld_a)) %>%
    ungroup() %>%
    group_by(Community, type) %>%
    summarise(prob_epi = round(sum(infections>=threshold)/params1$Nruns,2),
              av_start_time = mean(start)) %>%
    mutate(row = row[Community],
           col = col[Community]) -> summary_stats1
  
  excluded <- setdiff(1:100,summary_stats1$Community)
  type_excluded <- ifelse(excluded %in% city, "City","Non-city")
  
  bind_cols("Community"=excluded,
            "type"=type_excluded %>% as.character(),
            "prob_epi"=rep(0,length(excluded)),
            "av_start_time"=rep(NA,length(excluded)),
            "row"=row[excluded],
            "col"=col[excluded]) %>%
    as_tibble() %>%
    bind_rows(summary_stats1)-> summary_stats1_complete
  
  
  
  heatmap1 <- ggplot(summary_stats1_complete,aes(x=col,y=row)) +
    geom_tile(aes(fill=prob_epi,color=type),size=2,width=0.8,height=0.8) +
    #scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_color_grey(start = 1, end = 0)+
    geom_text(aes(label = prob_epi), fontface = "bold",color="white") +
    #scale_fill_viridis_c(limits=c(0,1)) + 
    theme_classic() +
    theme(legend.position = "bottom", axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_blank()) + scale_alpha(guide = 'none') +
    labs(fill=paste0("probability of ",threshold, " cases by day ", day),
         color=element_blank(),
         alpha=element_blank())
    # labs(fill=paste0("% change in probability of ",threshold, " cases by day ", day),
    #      color=element_blank(),
    #      alpha=element_blank(),
    #      caption=paste0("Comparing relative travel after restrictions announced: ", params2$alpha_inc, " to ", params1$alpha_inc,
    #                     "\nCases to trigger restrictions = ", params1$cases_ld_a,
    #                     "\nDays between restrictions annnounced and begin = ",params1$ld_b, 
    #                     "\nRelative beta after restrictions announced = ", params1$beta_inc, "; Relative beta after restrictions in place = ", params1$beta_dec, 
    #                     "\nInitial movement prob = ",params1$alpha_init, "; Relative travel after restrictions in place = ", params1$alpha_dec))
  
  heatmap2 <- ggplot(summary_stats1_complete,aes(x=col,y=row)) +
    geom_tile(aes(fill=av_start_time,color=type),size=2,width=0.8,height=0.8) +
    #scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_color_grey(start = 1, end = 0)+
    geom_text(aes(label = round(av_start_time)), fontface = "bold",color="white") +
    #scale_fill_gradient(high = "red", low = muted("green")) +
    #scale_fill_viridis_c(limits=c(0,1)) + 
    # scale_fill_gradient2(
    #   low = muted("red"),
    #   mid = "white",
    #   high = muted("blue"),
    #   midpoint = 0,
    #   space = "Lab",
    #   na.value = "grey",
    #   guide = "colourbar",
    #   aesthetics = "fill"
    # )+
    theme_classic() +
    theme(legend.position = "bottom", axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_blank()) + scale_alpha(guide = 'none')+
    labs(fill="Average start time",
         color=element_blank(),
         alpha=element_blank())
    # labs(fill=paste0("% change in start time "),
    #      color=element_blank(),
    #      alpha=element_blank(),
    #      caption=paste0("Comparing relative travel after restrictions announced: ", params2$alpha_inc, " to ", params1$alpha_inc,
    #                     "\nCases to trigger restrictions = ", params1$cases_ld_a,
    #                     "\nDays between restrictions annnounced and begin = ",params1$ld_b, 
    #                     "\nRelative beta after restrictions announced = ", params1$beta_inc, "; Relative beta after restrictions in place = ", params1$beta_dec, 
    #                     "\nInitial movement prob = ",params1$alpha_init, "; Relative travel after restrictions in place = ", params1$alpha_dec))
    # 
    # 
  list(heatmap1,heatmap2)
  
}

save_plot <- function(p, plot_path){
  ggsave(plot_path, p, dpi = 300)
}

generate_plots <- function(run_all = FALSE){

  if(run_all){
    plot_list <- dir("./cache/results/") %>% str_remove(".rds") %>% unique()
  }else{
    #load all results in cache
    current_plots <- dir("./cache/figs/") %>% str_remove(".png") %>% unique()
    current_data <- dir("./cache/results/") %>% str_remove(".rds") %>% unique()
    
    plot_list <- current_data[!current_data %in% current_plots] 
  }
  
  lapply(plot_list, function(x) save_plot(p = create_heatmap(x), plot_path = paste0("./cache/figs/",x,".png")))
  
}


summary_stats <- function(row){
  
    rds_id <- read_csv("./results_log.csv") %>% subset(unique_id == unique_id[row]) %>% pull(unique_id)
  
    suppressMessages(
      params <- read_csv("./results_log.csv") %>% subset(unique_id == rds_id)
    )
    
    data <- read_rds(paste0("./cache/results/",rds_id,".rds"))
    
    data %>%
      group_by(Community,Simulation, type) %>%
      summarise(infections = sum(n),
                start = min(DayInfected),
                t_ld_a = min(t_ld_a)) %>%
      ungroup() %>%
      group_by(Community, type) %>%
      summarise(prob_epi = round(sum(infections>=10)/params$Nruns,2),
                av_start_time = round(mean(start - t_ld_a),1)) %>%
      ungroup %>%
      summarise(av_prob_epi = mean(prob_epi),
             num_comm_epi = sum(prob_epi>0),
             av_start_time = mean(av_start_time))-> summary_stats
    
      bind_cols(params,summary_stats) -> summary_results_row
      
      return(summary_results_row)
  
}

# rows <- seq(17,400,1)
# 
# lapply(rows,function(i)summary_stats(i)) %>%
#   bind_rows() %>%
#   mutate(alpha_beta = paste0("travel inc: ",alpha_inc,
#                              "; beta inc: ", beta_inc,
#                              "; travel dec: ",alpha_dec,
#                              "; beta dec: ",beta_dec),
#          param_sums = paste0("initial: ",  num_inf,
#                              "; travel: ", alpha_init,
#                              "; trigger: ",cases_ld_a,
#                              "; delay: ",ld_b))-> summary_results
# 
# ggplot(summary_results,aes(x=param_sums,y=alpha_beta)) +
#   geom_tile(aes(fill=av_prob_epi),size=2,width=0.8,height=0.8) +
#   scale_color_grey(start = 1, end = 0)+
#   #geom_text(aes(label = round(av_start_time)), fontface = "bold",color="black") +
#   geom_text(aes(label = round(av_prob_epi,1)), fontface = "bold",color="black") +
#   scale_fill_viridis_c(limits=c(0,1)) + 
#   theme_classic() +
#   theme(legend.position = "bottom", axis.ticks = element_blank(),
#         axis.text.x=element_text(angle=90),
#         #axis.text.x = element_blank(),
#         #axis.text.y = element_blank(),   
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line = element_blank()) + scale_alpha(guide = 'none')+
#   labs(fill="Probability have 10+ cases",
#        #x="Other parameters",
#        #y="Travel & beta",
#        color=element_blank(),
#        alpha=element_blank())



create_heatmap3 <- function(rds_id1,rds_id2,comm_version,threshold,day){
  
  suppressMessages(
    params1 <- read_csv("./results_log.csv") %>% subset(unique_id == rds_id1)
  )
  
  data1 <- read_rds(paste0("./cache/results/",rds_id1,".rds"))
  
  suppressMessages(
    params2 <- read_csv("./results_log.csv") %>% subset(unique_id == rds_id2)
  )
  
  data2 <- read_rds(paste0("./cache/results/",rds_id2,".rds"))
  
  
  comm_type_list <- get_comm_types(params1$num_communities,comm_version)
  
  city <- comm_type_list[[1]]
  noncity <- c(comm_type_list[[2]],comm_type_list[[3]])
  
  data1$type <- ifelse(data1$Community %in% city, "City","Non-city")
  data2$type <- ifelse(data2$Community %in% city, "City","Non-city")
  
  num_edge <- sqrt(params1$num_communities)
  row <- c(rep(1:num_edge,each=10))
  col <- c(rep(1:num_edge,times=10))
  
  # summarise total # infections and start times by community
  
  data1 %>%
    group_by(Simulation,DayInfected, Community, type, t_ld_a) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Community) %>%
    mutate(cumulative=cumsum(n)) %>%
    ungroup() -> data1
  
  data1 %>%
    filter(DayInfected<=day) %>%
    group_by(Community,Simulation, type) %>%
    summarise(infections = sum(n),
              start = min(DayInfected),
              t_ld_a = min(t_ld_a)) %>%
    ungroup() %>%
    group_by(Community, type) %>%
    summarise(prob_epi = round(sum(infections>=threshold)/params1$Nruns,2),
              av_start_time = mean(start)) %>%
    mutate(row = row[Community],
           col = col[Community]) -> summary_stats1
  
  excluded <- setdiff(1:100,summary_stats1$Community)
  type_excluded <- ifelse(excluded %in% city, "City","Non-city")
  
  bind_cols("Community"=excluded,
            "type"=type_excluded %>% as.character(),
            "prob_epi"=rep(0,length(excluded)),
            "av_start_time"=rep(NA,length(excluded)),
            "row"=row[excluded],
            "col"=col[excluded]) %>%
    as_tibble() %>%
    bind_rows(summary_stats1)-> summary_stats1_complete
  
  data2 %>%
    group_by(Simulation,DayInfected, Community, type, t_ld_a) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    group_by(Community) %>%
    mutate(cumulative=cumsum(n)) %>%
    ungroup() -> data2
  
  data2 %>%
    filter(DayInfected<=day) %>%
    group_by(Community,Simulation, type) %>%
    summarise(infections = sum(n),
              start = min(DayInfected),
              t_ld_a = min(t_ld_a)) %>%
    ungroup() %>%
    group_by(Community, type) %>%
    summarise(prob_epi = round(sum(infections>=threshold)/params1$Nruns,2),
              av_start_time = mean(start)) %>%
    mutate(row = row[Community],
           col = col[Community]) -> summary_stats2
  
  excluded <- setdiff(1:100,summary_stats2$Community)
  type_excluded <- ifelse(excluded %in% city, "City","Non-city")
  
  bind_cols("Community"=excluded,
            "type"=type_excluded %>% as.character(),
            "prob_epi"=rep(0,length(excluded)),
            "av_start_time"=rep(NA,length(excluded)),
            "row"=row[excluded],
            "col"=col[excluded]) %>%
    as_tibble() %>%
    bind_rows(summary_stats2)-> summary_stats2_complete
  
  summary_stats2_complete %>%
    right_join(summary_stats1_complete, by="Community") %>%
    mutate(av_start_time.x=case_when(is.na(av_start_time.x)~0, TRUE~av_start_time.x),
           av_start_time.y=case_when(is.na(av_start_time.y)~0, TRUE~av_start_time.y),
           perc_change_label = round((prob_epi.x - prob_epi.y)/prob_epi.y*100,1),
           perc_change_time_label = round((av_start_time.x - av_start_time.y)/av_start_time.y*100,1)
    )-> summary_stats
  
  summary_stats$perc_change <- ifelse(is.infinite(summary_stats$perc_change_label), 
                                      max(summary_stats$perc_change_label[is.finite(summary_stats$perc_change_label)])*1.5,
                                      summary_stats$perc_change_label)
  
  summary_stats$perc_change_time <- ifelse(is.infinite(summary_stats$perc_change_time_label), 
                                           min(summary_stats$perc_change_time_label[is.finite(summary_stats$perc_change_time_label)])*1.5,
                                           summary_stats$perc_change_time_label)
  
  heatmap1 <- ggplot(summary_stats,aes(x=col.x,y=row.x)) +
    geom_tile(aes(fill=perc_change,color=type.x),size=2,width=0.8,height=0.8) +
    #scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_color_grey(start = 1, end = 0)+
    geom_text(aes(label = perc_change_label), fontface = "bold",color="black") +
    #scale_fill_gradient(high = "red", low = muted("green")) +
    #scale_fill_viridis_c(limits=c(-1,1)) + 
    scale_fill_gradient2(
      low = muted("blue"),
      mid = "white",
      high = muted("red"),
      midpoint = 0,
      space = "Lab",
      na.value = "grey",
      guide = "colourbar",
      aesthetics = "fill"
    )+
    theme_classic() +
    theme(legend.position = "bottom", axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_blank()) + scale_alpha(guide = 'none')+
    labs(fill=paste0("% change in probability of ",threshold, " cases by day ", day),
         color=element_blank(),
         alpha=element_blank(),
         caption=paste0("Comparing relative travel after restrictions announced: ", params2$alpha_inc, " to ", params1$alpha_inc,
                        "\nCases to trigger restrictions = ", params1$cases_ld_a,
                        "\nDays between restrictions annnounced and begin = ",params1$ld_b, 
                        "\nRelative beta after restrictions announced = ", params1$beta_inc, "; Relative beta after restrictions in place = ", params1$beta_dec, 
                        "\nInitial movement prob = ",params1$alpha_init, "; Relative travel after restrictions in place = ", params1$alpha_dec))
  
  heatmap2 <- ggplot(summary_stats,aes(x=col.x,y=row.x)) +
    geom_tile(aes(fill=perc_change_time,color=type.x),size=2,width=0.8,height=0.8) +
    #scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_color_grey(start = 1, end = 0)+
    geom_text(aes(label = perc_change_time_label), fontface = "bold",color="black") +
    #scale_fill_gradient(high = "red", low = muted("green")) +
    #scale_fill_viridis_c(limits=c(-1,1)) + 
    scale_fill_gradient2(
      low = muted("red"),
      mid = "white",
      high = muted("blue"),
      midpoint = 0,
      space = "Lab",
      na.value = "grey",
      guide = "colourbar",
      aesthetics = "fill"
    )+
    theme_classic() +
    theme(legend.position = "bottom", axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_blank()) + scale_alpha(guide = 'none')+
    labs(fill=paste0("% change in start time "),
         color=element_blank(),
         alpha=element_blank(),
         caption=paste0("Comparing relative travel after restrictions announced: ", params2$alpha_inc, " to ", params1$alpha_inc,
                        "\nCases to trigger restrictions = ", params1$cases_ld_a,
                        "\nDays between restrictions annnounced and begin = ",params1$ld_b, 
                        "\nRelative beta after restrictions announced = ", params1$beta_inc, "; Relative beta after restrictions in place = ", params1$beta_dec, 
                        "\nInitial movement prob = ",params1$alpha_init, "; Relative travel after restrictions in place = ", params1$alpha_dec))
  
  
  list(heatmap1,heatmap2)
  
}

results_log <- read_csv("./results_log.csv")

results_log %>%
  subset(alpha_init == 0.01 & cases_ld_a==10 & alpha_dec == 0.5 & beta_dec == 0.5) -> results

rds_id_alpha1.0_beta1.0 <- results %>% subset(alpha_inc==1 & beta_inc==1) %>% pull(unique_id) # should baseline be beta_inc == 1.5? 
rds_id_alpha1.5_beta1.0 <- results %>% subset(alpha_inc==1.5 & beta_inc==1) %>% pull(unique_id) 
rds_id_alpha2.0_beta1.0 <- results %>% subset(alpha_inc==2 & beta_inc==1) %>% pull(unique_id) 
rds_id_alpha2.5_beta1.0 <- results %>% subset(alpha_inc==2.5 & beta_inc==1) %>% pull(unique_id) 
rds_id_alpha3.0_beta1.0 <- results %>% subset(alpha_inc==3 & beta_inc==1) %>% pull(unique_id) 
rds_id_alpha1.0_beta1.5 <- results %>% subset(alpha_inc==1 & beta_inc==1.5) %>% pull(unique_id) 
rds_id_alpha1.5_beta1.5 <- results %>% subset(alpha_inc==1.5 & beta_inc==1.5) %>% pull(unique_id) 
rds_id_alpha2.0_beta1.5 <- results %>% subset(alpha_inc==2 & beta_inc==1.5) %>% pull(unique_id) 
rds_id_alpha2.5_beta1.5 <- results %>% subset(alpha_inc==2.5 & beta_inc==1.5) %>% pull(unique_id) 
rds_id_alpha3.0_beta1.5 <- results %>% subset(alpha_inc==3 & beta_inc==1.5) %>% pull(unique_id) 
rds_id_alpha1.0_beta2.0 <- results %>% subset(alpha_inc==1 & beta_inc==2) %>% pull(unique_id) 
rds_id_alpha1.5_beta2.0 <- results %>% subset(alpha_inc==1.5 & beta_inc==2) %>% pull(unique_id) 
rds_id_alpha2.0_beta2.0 <- results %>% subset(alpha_inc==2 & beta_inc==2) %>% pull(unique_id) 
rds_id_alpha2.5_beta2.0 <- results %>% subset(alpha_inc==2.5 & beta_inc==2) %>% pull(unique_id) 
rds_id_alpha3.0_beta2.0 <- results %>% subset(alpha_inc==3 & beta_inc==2) %>% pull(unique_id) 

baseline <- create_heatmap(rds_id_alpha1.0_beta1.0,3,1,30)
baseline_prob <- baseline[[1]]
baseline_time <- baseline[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha1.5_beta1.0,3,1,30)
alpha1.5_beta1.0_prob <- output[[1]]
alpha1.5_beta1.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.0_beta1.0,3,1,30)
alpha2.0_beta1.0_prob <- output[[1]]
alpha2.0_beta1.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.5_beta1.0,3,1,30)
alpha2.5_beta1.0_prob <- output[[1]]
alpha2.5_beta1.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha3.0_beta1.0,3,1,30)
alpha3.0_beta1.0_prob <- output[[1]]
alpha3.0_beta1.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha1.0_beta1.5,3,1,30)
alpha1.0_beta1.5_prob <- output[[1]]
alpha1.0_beta1.5_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha1.5_beta1.5,3,1,30)
alpha1.5_beta1.5_prob <- output[[1]]
alpha1.5_beta1.5_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.0_beta1.5,3,1,30)
alpha2.0_beta1.5_prob <- output[[1]]
alpha2.0_beta1.5_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.5_beta1.5,3,1,30)
alpha2.5_beta1.5_prob <- output[[1]]
alpha2.5_beta1.5_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha3.0_beta1.5,3,1,30)
alpha3.0_beta1.5_prob <- output[[1]]
alpha3.0_beta1.5_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha1.0_beta2.0,3,1,30)
alpha1.0_beta2.0_prob <- output[[1]]
alpha1.0_beta2.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha1.5_beta2.0,3,1,30)
alpha1.5_beta2.0_prob <- output[[1]]
alpha1.5_beta2.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.0_beta2.0,3,1,30)
alpha2.0_beta2.0_prob <- output[[1]]
alpha2.0_beta2.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha2.5_beta2.0,3,1,30)
alpha2.5_beta2.0_prob <- output[[1]]
alpha2.5_beta2.0_time <- output[[2]]

output <- create_heatmap3(rds_id_alpha1.0_beta1.0,rds_id_alpha3.0_beta2.0,3,1,30)
alpha3.0_beta2.0_prob <- output[[1]]
alpha3.0_beta2.0_time <- output[[2]]

panel_prob <- ggarrange(baseline_prob,alpha1.5_beta1.0_prob,alpha2.0_beta1.0_prob,alpha2.5_beta1.0_prob,alpha3.0_beta1.0_prob,
                   alpha1.0_beta1.5_prob,alpha1.5_beta1.5_prob,alpha2.0_beta1.5_prob,alpha2.5_beta1.5_prob,alpha3.0_beta1.5_prob,
                   alpha1.0_beta2.0_prob,alpha1.5_beta2.0_prob,alpha2.0_beta2.0_prob,alpha2.5_beta2.0_prob,alpha3.0_beta2.0_prob,
                   nrow=5)

panel_time <- ggarrange(baseline_time,alpha1.5_beta1.0_time,alpha2.0_beta1.0_time,alpha2.5_beta1.0_time,alpha3.0_beta1.0_time,
                        alpha1.0_beta1.5_time,alpha1.5_beta1.5_time,alpha2.0_beta1.5_time,alpha2.5_beta1.5_time,alpha3.0_beta1.5_time,
                        alpha1.0_beta2.0_time,alpha1.5_beta2.0_time,alpha2.0_beta2.0_time,alpha2.5_beta2.0_time,alpha3.0_beta2.0_time,
                        nrow=5)



