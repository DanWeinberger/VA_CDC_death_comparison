
call_inla <- function(label=c('cdc','va'), ds){
  combine.data <- list(label=ds)
  
  
  mod1 <- inla_model3(ds) #pblapply(combine.data, inla_model3)
  
  
  res1 <- mod1$preds_covars.m %>%
    mutate(year=lubridate::year(date)) %>%
    full_join( y=std.pop.VA.age.region.sex.race, by=c('agec','sex','region','race_recode')) %>%
    mutate(std.pop.va = ifelse(is.na(std.pop.va), 0.5, std.pop.va))
  

  #We can then take the results from the INLA and aggregate over different groups. For example, by age, by age/race, by age/region, by age/race/region:

  preds.age.sex.overall <- res1[res1$date>='2020-04-01',] %>%
    group_by(source,agec, sex, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec,sex) %>%
    summarize_grps_quantiles
  
  preds.age.overall <- res1[res1$date>='2020-04-01',] %>%
    group_by(source,agec,  variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec) %>%
    summarize_grps_quantiles
  
  preds.overall <- res1[res1$date>='2020-04-01',] %>%
    group_by(source,  variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source) %>%
    summarize_grps_quantiles
   
  preds.date <- res1 %>%
    group_by(source, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source, date) %>%
    summarize_grps_quantiles
  # select(source, agec ,date, starts_with('RR') , starts_with('excess'), starts_with('pred'), starts_with('log_RR'))
  
  preds.age <- res1 %>%
    group_by(source,agec, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec, date) %>%
    summarize_grps_quantiles
  
  preds.sex <- res1 %>%
    group_by(source,sex, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,sex, date) %>%
    summarize_grps_quantiles
   # select(source, agec ,date, starts_with('RR') , starts_with('excess'), starts_with('pred'), starts_with('log_RR'))
  
  preds.age_race <- res1 %>%
    group_by(source,agec, race_recode, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec,race_recode, date) %>%
    summarize_grps_quantiles
  
  preds.age.region <- res1 %>%
    group_by(source,agec,region, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec,region, date) %>%
    summarize_grps_quantiles
  
  preds.age.region.sex <- res1 %>%
    group_by(source,agec,region,sex, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec,region,sex, date) %>%
    summarize_grps_quantiles
  
  preds.age.race.region <- res1[res1$year==2020,] %>%
    group_by(source,agec,region,race_recode,sex, date, variable) %>%
    summarize_grps_sums %>%
    ungroup() %>%
    group_by(source,agec,region,race_recode,sex, date) %>%
    summarize_grps_quantiles 
  
  
  # ##FOR NOW< IGNORE MISSING RACE CATEGORY WHEN CALCULATING STD RATES

  
  #Export summary measures
  preds.age.out <- preds.age %>%
    select(source, agec, date, pop, starts_with('RR'),starts_with('excess_inc'),starts_with('excess_std_inc'), starts_with('log_RR'),starts_with('obs_inc'),starts_with('obs_std_inc') )
  
  preds.sex.out <- preds.sex %>%
    select(source, sex, date, pop, starts_with('RR'),starts_with('excess_inc'),starts_with('excess_std_inc'), starts_with('log_RR'),starts_with('obs_inc'),starts_with('obs_std_inc') )
  
  preds.age.race.region.out <- preds.age.race.region %>%
    select(source, agec, region, race_recode, sex, date,pop, starts_with('RR'),starts_with('excess_inc'),starts_with('excess_std_inc'), starts_with('log_RR'),starts_with('obs_inc'),starts_with('obs_std_inc')  )
  
  preds.age_race.out <- preds.age_race %>% 
    select(source, agec, race_recode, date, pop, starts_with('RR'),starts_with('excess_inc'), starts_with('log_RR'),starts_with('obs_inc'),starts_with('obs_std_inc')  )
  
  preds.age.region.sex.out <- preds.age.region.sex %>%
    select(source, agec, region, sex, date, pop, starts_with('RR'),starts_with('excess_inc'), starts_with('log_RR'),starts_with('obs_inc'),starts_with('obs_std_inc')  )
  
  fit.plots <- mod.fit.plots(preds.age, preds.age.race.region,preds.age.region)
  rr.plot1 <- rr.plots(preds.age, preds.age.race.region,preds.age.region)
  saveRDS(fit.plots, paste0('./outputs/fit.plots.', label, '.rds'))
  saveRDS(rr.plot1, paste0('./outputs/rr.plots.', label, '.rds')) 
  
  save.list <- list('fit.plots'=fit.plots, 'rr.plot1'=rr.plot1,'preds.age'=preds.age.out,'preds.sex'=preds.sex.out,'preds.age.race.region'=preds.age.race.region.out,'preds.age_race'=preds.age_race.out, 'preds.date'= preds.date,'preds.overall'=preds.overall,'preds.age.overall'=preds.age.overall,'preds.age.sex.overall'=preds.age.sex.overall)
  saveRDS(save.list,paste0('./outputs/', 'summary_list_',label,'.rds'))
  
 
  
  plot.ds.list =list('fit.plots'=fit.plots, 'rr.plot1'=rr.plot1,'preds.age'=preds.age,'preds.age.region'=preds.age.region, 'preds.age.region.sex'=preds.age.region.sex,'preds.age_race'=preds.age_race, 'preds.age.race.region'=preds.age.race.region)
  
  return(plot.ds.list)
}