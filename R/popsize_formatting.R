census_popsize_format <- function(){
  pop1 <- read.csv('./Data/SC-EST2020-alldata6.csv')
  pop1$agec <- NA
  pop1$agec[pop1$AGE>=0 & pop1$AGE<1] <- '01'
  pop1$agec[pop1$AGE>=1 & pop1$AGE<5] <- '02'
  pop1$agec[pop1$AGE>=5 & pop1$AGE<15] <- '03'
  pop1$agec[pop1$AGE>=15 & pop1$AGE<25] <- '04'
  pop1$agec[pop1$AGE>=25 & pop1$AGE<35] <- '05'
  pop1$agec[pop1$AGE>=35 & pop1$AGE<45] <- '06' #category 85=85+; cat 999 is all combined
  pop1$agec[pop1$AGE>=45 & pop1$AGE<55] <- '07' #category 85=85+; cat 999 is all combined
  pop1$agec[pop1$AGE>=55 & pop1$AGE<65] <- '08' #category 85=85+; cat 999 is all combined
  pop1$agec[pop1$AGE>=65 & pop1$AGE<75] <- '09' #category 85=85+; cat 999 is all combined
  pop1$agec[pop1$AGE>=75 & pop1$AGE<85] <- '10' #category 85=85+; cat 999 is all combined
  pop1$agec[pop1$AGE>=85 & pop1$AGE<110] <- '11' #category 85=85+; cat 999 is all combined
  pop1$sex2 <- NA
  pop1$sex2[pop1$SEX==1] <- 'M'
  pop1$sex2[pop1$SEX==2]  <- 'F'
  pop1$sex2[pop1$SEX==0]  <- 'ALL'
  pop1$state.abb1 <- state.abb[match(pop1$NAME,state.name)]
  pop1$state.abb1[pop1$NAME=='District of Columbia'] <- 'DC'
  pop1$region <- NA
  pop1$region[pop1$state.abb1 %in% c('ME','VT','NH','MA','CT','RI','NY','NJ','PA')] <- 'Northeast'
  pop1$region[pop1$state.abb1 %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV','AL','MS','KY','TN','LA','OK','TX')] <- 'South'
  pop1$region[pop1$state.abb1 %in% c('AZ','CA','ID','NM','MT','UT','WY','NV','AK','CA','HI','OR','WA')] <- 'West'
  pop1$region[pop1$state.abb1 %in% c('IN','IL','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')] <- 'Midwest'
  
  pop1.m <- reshape2::melt(pop1[ pop1$ORIGIN==0 ,c('agec','region','RACE','sex2',"POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012", "POPESTIMATE2013",  "POPESTIMATE2014", "POPESTIMATE2015", "POPESTIMATE2016", "POPESTIMATE2017", "POPESTIMATE2018", "POPESTIMATE2019" , "POPESTIMATE2020" )], id.vars=c('agec','sex2', 'RACE','region'))
  pop2 <- reshape2::dcast(pop1.m, agec + sex2 +RACE +region+ variable ~., fun.aggregate = sum)
  pop2$year <- as.numeric(gsub( 'POPESTIMATE','',as.character(pop2$variable)))
  names(pop2) <- c('agec','SEX','RACE','region','variable','popsize','year')
  pop2 <- pop2[,c('agec','SEX','RACE','region','popsize','year')]
  pop2$qtr <- 1 #July 1
  #pop2 <- pop2[pop2$state !='US',]
  #This is a hacky fix because we only have pop estimates up until July 2020; assume it is constant until july 2021 for now
  fill_2021 <- pop2[pop2$year==2020,]
  fill_2021$year <- 2021
  pop2 <- bind_rows(pop2, fill_2021)
  empty_ts <- readRDS('./Data/Confidential/compiled_sex_age_race_qtr_region.rds')
  empty_ts <- empty_ts[,c('agec','sex','region','race_recode','year','qtr')]
  names(empty_ts) <- c('agec','sex','region','race','year','qtr')
  
  names(pop2) <- tolower(names(pop2))
  pop2 <- pop2[!is.na(pop2$agec),]
  pop2 <- pop2[pop2$sex !='ALL',]
  empty_ts <- empty_ts[empty_ts$agec !='12',]
  empty_ts$qtr <- as.numeric(empty_ts$qtr)
  empty_ts2 <- merge(empty_ts, pop2, by=c('agec','sex','race','region','year','qtr'), all=T)
  empty_ts2$date[empty_ts2$qtr==3] <- (paste(empty_ts2$year[empty_ts2$qtr==3] ,'07','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==4] <- (paste(empty_ts2$year[empty_ts2$qtr==4] ,'10','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==1] <- (paste(empty_ts2$year[empty_ts2$qtr==1] ,'01','01', sep='-'))
  empty_ts2$date[empty_ts2$qtr==2] <- (paste(empty_ts2$year[empty_ts2$qtr==2] ,'04','01', sep='-'))
  empty_ts2$date <- as.Date(empty_ts2$date)
  empty_ts2 <- empty_ts2[empty_ts2$date>='2013-07-01' ,] #UPDATE THIS!
  empty_ts2 <- empty_ts2[empty_ts2$race !=6,]
  filled_pop2 <- empty_ts2 %>%
    group_by(agec , sex,race, region) %>%
    arrange(agec , sex,race,region,year, qtr) %>%
    mutate(time=seq(1,n())) %>%
    mutate(pop.interpol=approx(time,popsize,time)$y)
  filled_pop2 <- filled_pop2[,c('agec','sex','race','region','date','pop.interpol')]
  
  
  
return(filled_pop2)
}

# test1 <- filled_pop2[filled_pop2$state=='NY' & filled_pop2$age_group=="75-84 years",]
# plot(test1$week_end, test1$pop.interpol)


#Check

# check1 <- filled_pop2 %>%
#   group_by(date,race) %>%
#   summarize(popsum=sum(pop.interpol))
