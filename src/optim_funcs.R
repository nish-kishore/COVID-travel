update_mob_data <- function(params,t,num_symp){
  with(params, {
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
        t >= t_ld_b ~ mob_net_norm2,
        # for now leaving same but could have a third one if want
        t >= t_ld_a & t <t_ld_b ~ mob_net_norm2
      )
      mob_net <- mob_net %>% matrix(nrow = 100, ncol = 100)
    }

    return(list(t_ld_a, t_ld_b, mob_net))
  })

}

update_disease_status <- function(params, t, num_asymp){
  with(params,{

    tmp_communities <- communities %>%
      mutate(
        alpha = case_when(
          iloc==start_comm & t >= t_ld_a & t < t_ld_b ~ alpha_init*alpha_inc,
          iloc==start_comm & t >= t_ld_b ~ alpha_init*alpha_dec,
          iloc!=start_comm & cum_symp >= cases_ld_a2 ~ alpha_init*alpha_dec,
          TRUE ~ alpha_init
        ),# change beta if threshold reached in other communnities but keep alpha the same
        beta = case_when(
          iloc==start_comm & t >= t_ld_a & t < t_ld_b ~ beta_init*beta_inc ,
          iloc==start_comm & t >= t_ld_b ~ beta_init*beta_dec,
          iloc!=start_comm & cum_symp >= cases_ld_a2 ~ beta_init*beta_dec,
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
        tot_num_exp = A+I,
        N_exp =  sum(rbinom(S,1,1-(1-beta_step)^tot_num_exp)),
        E = E + N_exp,
        S = S - N_exp
        )

    tmp_communities %>%
      dplyr::select(iloc, comm_type, N_asymp) %>%
      uncount(weights = N_asymp, .remove = FALSE) %>%
      dplyr::select(iloc, comm_type) %>%
      mutate(day = t, s = 0) %>%
      set_names(c("Community", "type", "DayInfected", "Symptoms")) -> asymp

    tmp_communities %>%
      dplyr::select(iloc, comm_type, N_symp) %>%
      uncount(weights = N_symp, .remove = FALSE) %>%
      dplyr::select(iloc, comm_type) %>%
      mutate(day = t, s = 1) %>%
      set_names(c("Community", "type", "DayInfected", "Symptoms")) -> symp

    tmp_communities %>%
      mutate(tot_pop = S+E+A+I+R) %>%
      dplyr::select(iloc, tot_pop) -> pop

    tmp_results <- rbind(symp, asymp)

    tmp_results$t_ld_a <- t_ld_a

    if(t == 1){
      tmp_results <- rbind(tibble("Community" = rep(start_comm,num_inf),
                                   "DayInfected" = rep(1,num_inf),
                                   "Symptoms" = c(rep(1,(num_inf - num_asymp)),rep(0,num_asymp)),
                                   "type" = "Urban",
                                   "t_ld_a" = 1000),
                   tmp_results)
    }

    start_comm_num_symp <- tmp_communities[start_comm, "cum_symp"] %>% pull()

    tmp_results <- left_join(tmp_results, pop, by = "iloc")

    return(list(tmp_communities, tmp_results, start_comm_num_symp))

  })
}

##--- UPDATE LOCATION
update_loc <- function(
  communities,
  t = t,
  mob_net = mob_net,
  num_communities
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

 moveS <- lapply(1:num_communities, function(x) sample(1:num_communities, pull(out_mob[x,"N_moveS"]), mob_net[x,], replace = T)) %>%
   unlist() %>% table() %>% enframe() %>% set_names(c("iloc", "diffS")) %>% mutate(iloc = as.integer(iloc), diffS = as.integer(diffS))

 moveE <- lapply(1:num_communities, function(x) sample(1:num_communities, pull(out_mob[x,"N_moveE"]), mob_net[x,], replace = T)) %>%
   unlist() %>% table() %>% enframe() %>% set_names(c("iloc", "diffE")) %>% mutate(iloc = as.integer(iloc), diffE = as.integer(diffE))

 moveA <- lapply(1:num_communities, function(x) sample(1:num_communities, pull(out_mob[x,"N_moveA"]), mob_net[x,], replace = T)) %>%
   unlist() %>% table() %>% enframe() %>% set_names(c("iloc", "diffA")) %>% mutate(iloc = as.integer(iloc), diffA = as.integer(diffA))

 moveR <- lapply(1:num_communities, function(x) sample(1:num_communities, pull(out_mob[x,"N_moveR"]), mob_net[x,], replace = T)) %>%
   unlist() %>% table() %>% enframe() %>% set_names(c("iloc", "diffR")) %>% mutate(iloc = as.integer(iloc), diffR = as.integer(diffR))

out <- out_mob %>%
  left_join(moveS, by = "iloc") %>%
  left_join(moveE, by = "iloc") %>%
  left_join(moveA, by = "iloc") %>%
  left_join(moveR, by = "iloc") %>%
  replace_na(list(diffS = 0, diffE = 0, diffA = 0, diffR = 0)) %>%
  mutate(S = S + diffS,
         E = E + diffE,
         A = A + diffA,
         R = R + diffR) %>%
  dplyr::select(-c(diffS, diffE, diffA, diffR))

  return(out)

}
