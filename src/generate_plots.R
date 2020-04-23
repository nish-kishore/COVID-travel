#scripts to generate plots
source("./src/dependencies.R")
source("./src/helper_functions.R")

generate_plots <- function(results_master, row){
  
  #load log of results generated so far 
  results_log <- read_csv("./results_log.csv")
  
  results_master <- load_run_results(results_log[row,"unique_id"])
  params <- results_log[row,] %>% as.list()

  num_edge <- sqrt(params$num_communities)
  row <- c(rep(1:num_edge,each=10))
  col <- c(rep(1:num_edge,times=10))
  
  urban <- c(45,57)
  suburban <-c(23:27,33:39,43:44,46:49,53:56,58:59,63:69,76:79)
  rural <- setdiff(1:params$num_communities, c(urban, suburban))
  
  # summarise total # infections and start times by community 
  results_master %>%
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
      geom_tile(aes(fill=prob_epi,color=type),size=2,width=0.8,height=0.8) + 
      scale_color_grey()+
      geom_text(aes(label = av_start_time)) +
      scale_fill_viridis_c(option="plasma",limits=c(0,1)) + theme_classic() + 
      theme(legend.position = "bottom", axis.ticks = element_blank(),
            axis.title.x = element_blank(),axis.title.y = element_blank(), 
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.line = element_blank()) + 
      labs(fill="Probability epidemic",
           color=element_blank(),
           caption="Number in cell denotes average time of first case \nrelative to announcement of restrictions") 
  
    return(heatmap)
}


