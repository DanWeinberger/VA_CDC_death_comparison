summarize_grps_quantiles <- function(ds){
  ds2 <- ds %>%
    summarise( 'N_deaths'=mean(N_deaths), 
               
               'excess_median'=sum(excess),
               'excess_median'=median(excess), 
               'excess_lcl' =quantile(excess,probs=0.025), 
               'excess_ucl'=quantile(excess,probs=0.975), 
               
               'pred_median'=median(pred),
               'pred_lcl'=quantile(pred,probs=0.025), 
               'pred_ucl'=quantile(pred,probs=0.975),
               
               'RR_median'=median(RR),
               'RR_lcl'=quantile(RR,probs=0.025),
               'RR_ucl'=quantile(RR,probs=0.975),
               
               'excess_inc'=median(excess_inc),
               'excess_inc_lcl'=quantile(excess_inc, probs=0.025),
               'exess_inc_ucl'= quantile(excess_inc, probs=0.975)
               )
  return(ds2)
}




summarize_grps_sums <- function(ds){
 ds1<- ds %>%
    summarise( 'N_deaths'=sum(N_deaths), 
               'pred'=sum(value), 
               'popsum'=sum(exp(log.offset1)*100000),
               'n_times_agg'=length(unique(date))) %>%
    mutate('RR'=N_deaths/pred,
           'pop'=popsum/n_times_agg,
           'excess'=(N_deaths - pred),
           'excess_inc'=(N_deaths - pred)/(popsum/n_times_agg)*100000 ) 
  return(ds1)
}
