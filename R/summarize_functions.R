summarize_grps_quantiles <- function(ds){
  ds %>%
    summarise( 'N_deaths'=mean(N_deaths), 
               'excess'=sum(excess),
               'pred_median'=median(pred),
               'excess_median'=median(excess), 
               'excess_lcl' =quantile(excess,probs=0.025), 
               'excess_ucl'=quantile(pred,probs=0.975), 
               'pred_lcl'=quantile(pred,probs=0.025), 
               'pred_ucl'=quantile(pred,probs=0.975),
               'RR'=median(RR),
               'RR_lcl'=quantile(RR,probs=0.025),
               'RR_ucl'=quantile(RR,probs=0.975)
               )
}




summarize_grps_sums <- function(ds){
  ds %>%
    summarise( 'N_deaths'=sum(N_deaths), 
               'pred'=sum(value), 
               'excess'=sum(N_deaths - value),
               'pop'=sum) %>%
    mutate('RR'=N_deaths/pred)
  
}