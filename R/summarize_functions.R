#this function summarizes things by group and for each Monte Carlo sample
summarize_grps_sums <- function(ds){
  ds1 <- ds %>%
    mutate('pop'=exp(log.offset1)*100000) %>%
    summarise( 'sum_N_deaths'=sum(N_deaths), 
               'pred'=sum(value), 
               'popsum'=sum(pop),
               'n_times_agg'=length(unique(date)) ,
               'sum_obs_std'= sum((N_deaths)*std.pop.va/sum(std.pop.va)/pop)*n_times_agg*100000, #* (4/n_times_agg),
               'sum_expected_std'= sum((value)*std.pop.va/sum(std.pop.va)/pop)*n_times_agg*100000, #* (4/n_times_agg),
               'sum_excess_std'= sum((N_deaths-value)*std.pop.va/sum(std.pop.va)/pop)*n_times_agg*100000 #* (4/n_times_agg),
               
               ) %>%
    mutate('RR'=(sum_N_deaths+0.5)/(pred+0.5),
           'Std_RR'= sum_obs_std/sum_expected_std,
           'pop'=popsum/n_times_agg,  #this is important if combining multiple time points
           'excess'=(sum_N_deaths - pred) ,
           'excess_inc'=(sum_N_deaths - pred)/(popsum/n_times_agg)*100000 ,#* (4/n_times_agg) ,
           'obs_inc'= sum_N_deaths/(popsum/n_times_agg)*100000 , #* (4/n_times_agg) ,
           'pred_inc'= pred/(popsum/n_times_agg)*100000  #* (4/n_times_agg)
    )
  
  return(ds1)
}

#Then take summary stats of the MCMC samples
summarize_grps_quantiles <- function(ds){
  ds2 <- ds %>%
    summarise( 'N_deaths'=mean(sum_N_deaths), 
               
               'obs_inc'= mean(obs_inc),
               
               'obs_std_inc'= mean(sum_obs_std),
               
               'expected_std_inc_median'= median(sum_expected_std),
               'expected_std_inc_lcl' =quantile(sum_expected_std,probs=0.025), 
               'expected_std_inc_ucl'=quantile(sum_expected_std,probs=0.975), 
               
               'excess_std_inc_median'=median(sum_excess_std), 
               'excess_std_inc_lcl' =quantile(sum_excess_std,probs=0.025), 
               'excess_std_inc_ucl'=quantile(sum_excess_std,probs=0.975),              
               
               'excess_median'=median(excess), 
               'excess_lcl' =quantile(excess,probs=0.025), 
               'excess_ucl'=quantile(excess,probs=0.975), 
               
               'pred_median'=median(pred),
               'pred_lcl'=quantile(pred,probs=0.025), 
               'pred_ucl'=quantile(pred,probs=0.975),
               
               'log_RR_median'=median(log(RR)),
               'log_RR_SD' =sd(log(RR)),
               
               'RR_median'=median(RR),
               'RR_lcl'=quantile(RR,probs=0.025),
               'RR_ucl'=quantile(RR,probs=0.975),
               
               'RR_std_median'=median(Std_RR),
               'RR_std_lcl'=quantile(Std_RR,probs=0.025),
               'RR_std_ucl'=quantile(Std_RR,probs=0.975),
               
               'excess_inc_median'=median(excess_inc),
               'excess_inc_lcl'=quantile(excess_inc, probs=0.025),
               'excess_inc_ucl'= quantile(excess_inc, probs=0.975),
               
               'obs_inc'=median(obs_inc),
               'pred_inc_median'=median(pred_inc),
               'pred_inc_lcl'=quantile(pred_inc,probs=0.025),
               'pred_inc_ucl'=quantile(pred_inc,probs=0.975),
               'pop'= median(pop)
               
                              )
  return(ds2)
}



