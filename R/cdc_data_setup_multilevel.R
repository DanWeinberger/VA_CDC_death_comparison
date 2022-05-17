
## Set up data for multilevel model

cdc_data_setup_multilevel <- function(){
  
  #RAW DATA READ USING READ_MortalityTapes.R
  ag3 <- readRDS('./Data/Confidential/compiled_sex_agec_race_qtr_region.rds')
  ag3$month <- NA
  ag3$month[ag3$qtr==1] <- 1
  ag3$month[ag3$qtr==2] <- 4
  ag3$month[ag3$qtr==3] <- 7
  ag3$month[ag3$qtr==4] <- 10
  ag3$date <- as.Date(paste(ag3$year, ag3$month, '01', sep='-'))
  ag3$qtr2 <- 0
  ag3$qtr2[ag3$qtr==2] <- 1
  ag3$qtr3 <- 0
  ag3$qtr3[ag3$qtr==3] <- 1
  ag3$qtr4 <- 0
  ag3$qtr4[ag3$qtr==4] <- 1
  #ag3 <- ag3[ag3$state!='US',]
  ag3$time <- round(as.numeric(ag3$date - min(ag3$date))/30.3)
  ag3$age_group <- as.factor(ag3$agec)
  ag3$race_recode <- as.factor(ag3$race_recode)
  ag3$sex <- as.factor(ag3$sex)
  #ag3$state <- as.factor(ag3$state)
  #mod.inla <- inla(all_cause ~  , family='poisson')
  
  ag3$time_scale <- as.vector(scale(ag3$time))
  # ag3.pre <- ag3[ag3$week_end < '2020-03-01',] %>%
  #   group_by(state,age_group) %>%
  #   summarize( log.ave.case = log(mean(all_cause)  ))
  
  #ag3 <- merge( ag3, ag3.pre, by=c('state','age_group'))
  
  filled_pop2 <- readRDS('./Data/pop_interpol.rds')
  filled_pop2$sex <- as.factor(filled_pop2$sex)

  filled_pop2$race_recode <- as.factor(filled_pop2$race_recode)
  
  ag3 <- merge(ag3, filled_pop2, by.x=c('sex','race_recode','agec','region', 'date'),by.y=c('sex','race_recode','agec','region' ,'date'), all.x=T)
  ag3$log_pop <- log(ag3$pop.interpol/100000)

  ag3$race_recode2 <- ag3$race_recode
  ag3$race_recode3 <- ag3$race_recode
  ag3$race_recode4 <- ag3$race_recode
  ag3$race_recode5 <- ag3$race_recode
  ag3$sex2 <- ag3$sex
  ag3$sex3 <- ag3$sex
  #ag3$state2 <- ag3$state
  #ag3$state2 <- ag3$state
  ag3 <- ag3[!is.na(ag3$pop.interpol),]
  ag3$obs <- as.factor(1:nrow(ag3))
  ag3$region <- as.factor(ag3$region)
  
  ag3$agec <- as.factor(ag3$agec)
  ag3$agec <- relevel(ag3$agec,'85 years and older') #set grp 11 as reference--this is highest incidence group and allow us to use N(0,1) priors for all covariates
  
  ag3 <- ag3[!is.na(ag3$region),]
  

  ag3$subgroup_combo <- as.factor(paste(ag3$race_recode, ag3$agec, ag3$sex,ag3$region))
  
  ag3$source <- 'cdc'

  ag3 <- ag3 %>%
    group_by(sex,race_recode,agec,region,date,year,qtr,source, time_scale, qtr2, qtr3, qtr4, subgroup_combo) %>%
    summarise('N_deaths'=sum(N_deaths), 'pop.interpol'=sum(pop.interpol))
  
  ag3$all_cause_pre <- ag3$N_deaths
  ag3$all_cause_pre[ag3$date >= '2020-01-01'] <- NA #bc it is quarterly data, Q1 includes pandeic period
return(ag3)
}
#ag3 <- ag3[ag3$year<=2020,]