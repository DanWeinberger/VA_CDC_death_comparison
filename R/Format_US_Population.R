
#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(Hmisc )
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

Format_US_Population <- function(){
  #these are all July 1 estimates of popsize for the year
  d1 <- read_fwf(file="./Data/pcen_v2020_y1020.txt",
                                   fwf_positions(start=c(5,10,12,13,46,54,62,70,78, 86,94,102),
                                                 end=c(  6,11,12,13,53,61,69,77,85, 93,101,109),
                                                 col_names = c('state','agey', 'race_sex', 'hispanic','pop2013','pop2014','pop2015','pop2016','pop2017','pop2018','pop2019','pop2020' )),
                                    guess_max=10000)
 
  d1$agec <- NA
  d1$agec[d1$agey >=0 & d1$agey<25] <- "Under 25 Years"
  d1$agec[d1$agey >=25 & d1$agey<45] <- "25-44 years"
  d1$agec[d1$agey >=45 & d1$agey<65] <- "45-64 years"
  d1$agec[d1$agey >=65 & d1$agey<75] <- "65-74 years"
  d1$agec[d1$agey >=75 & d1$agey<85] <- "75-84 years"
  d1$agec[d1$agey >=85 & d1$agey<115] <- '85 years and older'
  
  d1$statecode <- fips(d1$state, to="Abbreviation" )
  
  d1$region <- NA
  d1$region[d1$statecode %in% c('ME','VT','NH','MA','CT','RI','NY','NJ','PA')] <- 'Northeast'
  d1$region[d1$statecode %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV','AL','MS','KY','TN','LA','OK','TX', 'AR')] <- 'South'
  d1$region[d1$statecode %in% c('AZ','CA','CO','ID','NM','MT','UT','WY','NV','AK','CA','HI','OR','WA')] <- 'West'
  d1$region[d1$statecode %in% c('IN','IL','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')] <- 'Midwest'
  
  # RACE_RECODE: 
  #   1=Non-Hispanic White
  # 2=Non-Hispanic- Black
  # 3= Hispanic
  # 4=American Indian/Native Alaskan
  # 5= Asian/Pacific Island
  # 999=Missing
  
  d1$race_recode <- 999
  d1$race_recode[d1$race_sex %in% c(1,2) & d1$hispanic==1] <- 1
  d1$race_recode[d1$race_sex %in% c(3,4) & d1$hispanic==1] <- 2
  d1$race_recode[d1$hispanic==2] <- 3
  d1$race_recode[d1$race_sex %in% c(5,6) & d1$hispanic==1] <- 4
  d1$race_recode[d1$race_sex %in% c(7,8) & d1$hispanic==1] <- 5
  
  d1$sex <- NA
  d1$sex[d1$race_sex %in% c(1,3,5,7)] <- 'M'
  d1$sex[d1$race_sex %in% c(2,4,6,8)] <- 'F'
  
  d2 <- d1 %>%
    group_by(agec, region, race_recode, sex) %>%
    dplyr::summarize('pop2013'=sum(pop2013),
              'pop2014'=sum(pop2014),
              'pop2015'=sum(pop2015),
              'pop2016'=sum(pop2016),
              'pop2017'=sum(pop2017),
              'pop2018'=sum(pop2018),
              'pop2019'=sum(pop2019),
              'pop2020'=sum(pop2020)) %>%
    ungroup()
  
  summary_stats <- d1 %>%
    #filter(agey>=25) %>%
    dplyr::summarize( ave_age= wtd.mean(agey, pop2019),
                      ave.age_sd= sqrt(wtd.var(agey, pop2019)),
                      popsize=sum(pop2019)
      )
  
  summary_stats2 <- d1 %>%
    group_by(agec) %>%
    dplyr::summarize(  popsize=sum(pop2019)    ) %>%
    ungroup() %>%
    mutate(pct= popsize/sum(popsize))
  
  summary_stats3 <- d1 %>%
    group_by(race_recode) %>%
    dplyr::summarize(  popsize=sum(pop2019)    ) %>%
    ungroup() %>%
    mutate(pct= popsize/sum(popsize))
  
  summary_stats4 <- d1 %>%
    group_by(sex) %>%
    dplyr::summarize(  popsize=sum(pop2019)    ) %>%
    ungroup() %>%
    mutate(pct= popsize/sum(popsize))
  
  
  d2.m <- melt(d2, id.vars=c('agec','region','race_recode','sex'))
  
  names(d2.m) <- c('agec','region','race_recode','sex','variable','pop')
  
  d2.m$year <- as.numeric(substring(d2.m$variable,4))
  
  fill_2021 <- d2.m[d2.m$year==2020,]
  fill_2021$year <- 2021
  pop2 <- bind_rows(d2.m, fill_2021)
  
  pop2$qtr <- 2
  
  empty_ts <- readRDS('./Data/Confidential/compiled_sex_agec_race_qtr_region.rds')
  empty_ts <- empty_ts[,c('agec','sex','region','race_recode','year','qtr')]
  names(empty_ts) <- c('agec','sex','region','race_recode','year','qtr')
  
  names(pop2) <- tolower(names(pop2))
  pop2 <- pop2[!is.na(pop2$agec),]
  pop2 <- pop2[pop2$sex !='ALL',]

  empty_ts$qtr <- as.numeric(empty_ts$qtr)
  
  empty_ts2 <- merge(empty_ts, pop2, by=c('agec','sex','race_recode','region','year','qtr'), all=T)
  
  empty_ts2$date[empty_ts2$qtr==3] <- (paste(empty_ts2$year[empty_ts2$qtr==3] ,'07','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==4] <- (paste(empty_ts2$year[empty_ts2$qtr==4] ,'10','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==1] <- (paste(empty_ts2$year[empty_ts2$qtr==1] ,'01','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==2] <- (paste(empty_ts2$year[empty_ts2$qtr==2] ,'04','01', sep='-'))
  empty_ts2$date <- as.Date(empty_ts2$date)
  empty_ts2 <- empty_ts2[empty_ts2$date>='2013-04-01' ,] #UPDATE THIS!

  empty_ts2 <-  empty_ts2[!is.na(empty_ts2$agec),] 
  empty_ts2 <-  empty_ts2[empty_ts2$race_recode != 999,] 
  
  filled_pop2 <- empty_ts2 %>%
    group_by(agec , sex,race_recode, region) %>%
    arrange(agec , sex,race_recode,region,year, qtr) %>%
    #mutate(time=seq(1,n())) %>%  #FIX THIS--wont be correct for 2013
    mutate(time= year + qtr/4 - 1/4 ) %>%
    mutate(pop.interpol=approx(time,pop,time)$y) %>%
    ungroup()
  
  filled_pop2 <- filled_pop2[,c('agec','sex','race_recode','region','date','pop.interpol')]
  
  saveRDS(filled_pop2,'./Data/pop_interpol.rds')

  std.pop1 <- filled_pop2[filled_pop2$date=='2020-01-01',]
  names(std.pop1) <- c('agec','sex','race_recode','region','std.pop.date','std.pop')
  
  saveRDS(std.pop1,'./Data/std.pop.age.region.sex.race.rds')
  
  std.pop2 <- std.pop1 %>%
    group_by(agec, sex, race_recode) %>%
    dplyr::summarize('std.pop'=sum(std.pop)) %>%
    ungroup()
  
  saveRDS(std.pop2,'./Data/std.pop.age.sex.race.rds')
  
  }