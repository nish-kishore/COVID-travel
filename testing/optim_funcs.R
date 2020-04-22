update_mob_data <- function(t_ld_a, t_ld_b, ld_b,t,num_symp){
  # if lockdown hasn't been announced yet check if it should be
  if (t_ld_a==1000){
    # track symptomatic infections 
    if (num_symp >= cases_ld_a){
      t_ld_a <- t
      t_ld_b <- t + ld_b
    }
    mob_net <- mob_net_norm
  } else { 
    mob_net <- case_when(
      t > t_ld_b ~ mob_net_norm2, 
      # for now leaving same but could have a third one if want
      t>=t_ld_a & t <t_ld_b ~ mob_net_norm2
    )
  }
  
  return(list(t_ld_a, t_ld_b, mob_net))
  
}

update_disease_status <- function(communities, params, t){
  params$communities <- communities
  with(params,{

    tmp_communities <- communities %>%
      mutate(
        alpha = case_when(
          iloc==start_comm & t >= t_ld_a & t < t_ld_b ~ alpha_init*alpha_inc,
          iloc==start_comm & t >= t_ld_b ~ alpha_init*alpha_dec,
          TRUE ~ alpha_init
        ),# change beta if threshold reached in other communnities but keep alpha the same
        beta = case_when(
          iloc==start_comm & t >= t_ld_a & t < t_ld_b ~ beta_init*beta_inc ,
          iloc==start_comm & t >= t_ld_b ~ beta_init*beta_dec,
          cum_symp >= cases_ld_a ~ beta_init*beta_dec,
        TRUE ~ beta_init
        ),
        beta_step = beta/area) %>%
      rowwise() %>%
      mutate(#recover
        recover_AS = sum(rbinom(A, 1, rec_per)),
        recover_S = sum(rbinom(I, 1, rec_per)),
        A = A - recover_AS,
        I = I - recover_S,
        R = R + recover_AS + recover_S,#exposed --> infected
        num_new_inf = sum(rbinom(E, 1, lat_per)),
        N_asymp = sum(rbinom(num_new_inf,1,perc_asymp)),
        N_symp = num_new_inf - N_asymp,
        cum_symp = cum_symp + N_symp,
        E = E - num_new_inf,
        A = A + N_asymp,
        I = I + (num_new_inf - N_asymp), #susceptile --> exposed
        N_exp =  sum(rbinom(S,1,1-(1-(beta/(S+E+A+I+R)))^(A+I))),
        E = E + N_exp,
        S = S - N_exp
        ) 
    
    tmp_communities %>%
      dplyr::select(iloc, N_asymp) %>%
      uncount(weights = N_asymp, .remove = FALSE) %>%
      dplyr::select(iloc) %>%
      mutate(day = t, s = 0) %>%
      set_names(c("Community", "DayInfected", "Symptoms")) -> asymp
    
    tmp_communities %>%
      dplyr::select(iloc, N_symp) %>%
      uncount(weights = N_symp, .remove = FALSE) %>%
      dplyr::select(iloc) %>%
      mutate(day = t, s = 1) %>%
      set_names(c("Community", "DayInfected", "Symptoms")) -> symp
    
    tmp_results <- rbind(symp, asymp)
    
    if(t == 1){
      tmp_results <- rbind(tibble("Community" = rep(start_comm,num_inf),
                                   "DayInfected" = rep(1,num_inf),
                                   "Symptoms" = c(rep(1,(num_inf - num_asymp)),rep(0,num_asymp))),
                   tmp_results)
    }

    start_comm_num_symp <- tmp_communities[start_comm, "cum_symp"]
    
    return(list(tmp_communities, tmp_results, start_comm_num_symp))

  })
}

##--- UPDATE LOCATION
update_loc <- function(
  communities, 
  t = t,
  mob_net = mob_net
){
  
 out_mob <- communities %>%
    rowwise() %>%
    mutate(
      #move
      N_moveS = sum(rbinom(S, 1, alpha)),
      N_moveE = sum(rbinom(E, 1, alpha)), 
      N_moveA = sum(rbinom(A, 1, alpha)), 
      N_moveR = sum(rbinom(R, 1, alpha)), 
      S = S - N_moveS, 
      E = E - N_moveE, 
      A = A - N_moveA, 
      R = R - N_moveR
    ) 
 
 lapply(1:num_communities, function(x) sample(1:num_communities, pull(out_mob[x,"N_moveS"]), mob_net[x,], replace = T)) %>% 
   unlist() %>%
   table()
  
  for(j in c("S", "E", "A", "R")){
    new_mvmt <- tibble()
    for(x in 1:nrow(world)){
      if(tmp_world[x,] %>% pull(paste0("N_move",j)) > 0){
        out <- sample(c(1:nrow(world)),
                      tmp_world[x,] %>% pull(paste0("N_move",j)),
                      subset(mob_net, i == x) %>% pull("val"),
                      replace = TRUE) %>%
          table() %>% as_tibble() %>% set_names(c("id", "count")) 
        
        new_mvmt <- rbind(new_mvmt, out)
      }
    }
    if(nrow(new_mvmt)>0){
      out <- new_mvmt %>%
        group_by(id) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(id = as.integer(id)) %>%
        set_names(c("id", paste0("new",j))) 
      
      tmp_world <- left_join(tmp_world, out, by = "id")
      
    }else{
      out <- tibble() %>%
        mutate(id = 0, count = NA) %>%
        set_names("id", paste0("new", j))
      
      tmp_world <- left_join(tmp_world, out, by = "id")
    }
    
  }
  
  tmp_world %>%
    mutate(S = sum(S, newS, na.rm = T), 
           E = sum(E, newE, na.rm = T), 
           A = sum(A, newA, na.rm = T), 
           R = sum(R, newR, na.rm = T)) %>%
    dplyr::select(id, S, E, A, I, R) %>%
    as.matrix() -> out
  
  return(out)
  
}
