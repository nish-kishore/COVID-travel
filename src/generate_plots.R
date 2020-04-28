#scripts to generate plots
source("./src/helper_functions.R")

create_heatmap <- function(rds_id){
  
  suppressMessages(
    params <- read_csv("./results_log.csv") %>% subset(unique_id == rds_id)
  )
  
  data <- read_rds(paste0("./cache/results/",rds_id,".rds"))
  
  comm_type_list <- get_comm_types(params$num_communities)
  
  urban <- comm_type_list[[1]]
  suburban <- comm_type_list[[2]]
  rural <- comm_type_list[[3]]
  
  num_edge <- sqrt(params$num_communities)
  row <- c(rep(1:num_edge,each=10))
  col <- c(rep(1:num_edge,times=10))
  
  # summarise total # infections and start times by community
  data %>%
    group_by(Community,Simulation, type) %>%
    summarise(infections = sum(n),
              start = min(DayInfected),
              t_ld_a = min(t_ld_a)) %>%
    ungroup() %>%
    group_by(Community, type) %>%
    summarise(prob_epi = round(sum(infections>=params$cases_ld_a)/params$Nruns,2),
              av_start_time = round(mean(start - t_ld_a),1)) %>%
    mutate(row = row[Community],
           col = col[Community]) -> summary_stats
  
  excluded <- setdiff(1:100,summary_stats$Community)
  type_excluded <- ifelse(excluded %in% urban, "Urban",
                          ifelse(excluded %in% suburban, "Suburban", "Rural"))
  
  bind_cols("Community"=excluded,
            "type"=type_excluded,
            "prob_epi"=rep(0,length(excluded)),
            "av_start_time"=rep(NA,length(excluded)),
            "row"=row[excluded],
            "col"=col[excluded]) %>%
    as_tibble() %>%
    bind_rows(summary_stats)-> summary_stats_complete
  
  heatmap <- ggplot(summary_stats_complete,aes(x=col,y=row)) +
    geom_tile(aes(fill=prob_epi,color=type,alpha=0.5),size=2,width=0.8,height=0.8) +
    #scale_color_brewer(type = "qual", palette = "Dark2") +
    scale_color_grey(start = 1, end = 0)+
    geom_text(aes(label = av_start_time), fontface = "bold",color="black") +
    #scale_fill_gradient(high = "red", low = muted("green")) +
    scale_fill_viridis_c(limits=c(0,1)) + 
    theme_classic() +
    theme(legend.position = "bottom", axis.ticks = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.line = element_blank()) + scale_alpha(guide = 'none')+
    labs(fill="Probability epidemic",
         color=element_blank(),
         alpha=element_blank(),
         caption=paste0("Number in cell denotes average time of first case relative to announcement of restrictions",
                       "\nCases to trigger restrictions = ", params$cases_ld_a, "; Days between restrictions annnounced and begin = ",params$ld_b, 
                       "\nRelative beta after restrictions announced = ", params$beta_inc, "; Relative beta after restrictions in place = ", params$beta_dec, 
                       "\nInitial movement prob = ",params$alpha_init,"; Relative travel after restrictions announced = ", params$alpha_inc,"; Relative travel after restrictions in place = ", params$alpha_dec))
  
  
  return(heatmap)
  
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

rows <- seq(17,400,1)

lapply(rows,function(i)summary_stats(i)) %>%
  bind_rows() %>%
  mutate(alpha_beta = paste0("travel inc: ",alpha_inc,
                             "; beta inc: ", beta_inc,
                             "; travel dec: ",alpha_dec,
                             "; beta dec: ",beta_dec),
         param_sums = paste0("initial: ",  num_inf,
                             "; travel: ", alpha_init,
                             "; trigger: ",cases_ld_a,
                             "; delay: ",ld_b))-> summary_results

ggplot(summary_results,aes(x=param_sums,y=alpha_beta)) +
  geom_tile(aes(fill=av_prob_epi),size=2,width=0.8,height=0.8) +
  scale_color_grey(start = 1, end = 0)+
  #geom_text(aes(label = round(av_start_time)), fontface = "bold",color="black") +
  geom_text(aes(label = round(av_prob_epi,1)), fontface = "bold",color="black") +
  scale_fill_viridis_c(limits=c(0,1)) + 
  theme_classic() +
  theme(legend.position = "bottom", axis.ticks = element_blank(),
        axis.text.x=element_text(angle=90),
        #axis.text.x = element_blank(),
        #axis.text.y = element_blank(),   
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank()) + scale_alpha(guide = 'none')+
  labs(fill="Probability have 10+ cases",
       #x="Other parameters",
       #y="Travel & beta",
       color=element_blank(),
       alpha=element_blank())

