process_nchs_region <- function(ds){
  age1 <- ds
  age1 <- age1[ age1$Type=='Unweighted' & age1$Suppress =='',]
  age1 <- age1[,c('Week.Ending.Date', 'Age.Group','Number.of.Deaths', 'State.Abbreviation' )]
  age1$Week.Ending.Date <- as.Date(age1$Week.Ending.Date, '%m/%d/%Y')
  
  
  #Cut off last 2 months of data--incomplete reporting
  max.date <- max(age1$Week.Ending.Date)
  age1 <- age1[age1$Week.Ending.Date<= (max.date-60),]
  
  names(age1) <-c('week_end','age_group','all_cause','state')
  age1$one <-1
  
  #age1$age_group[age1$age_group=="Under 25 years"] <- '0-25 years'
  
  age1 <- age1[age1$age_group %in% c('25-44 years' ,'45-64 years','65-74 years','75-84 years' ,  '85 years and older'),]
  
  age1$region[age1$state %in% c('ME','VT','NH','MA','CT','RI','NY','NJ','PA')] <- 'Northeast'
  age1$region[age1$state %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV','AL','MS','KY','TN','AK','LA','OK','TX')] <- 'South'
  age1$region[age1$state %in% c('AZ','CA','ID','NM','MT','UT','WY','NV','AK','CA','HI','OR','WA')] <- 'West'
  age1$region[age1$state %in% c('IN','IL','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')] <- 'Midwest'
  
  age1.region <- age1 %>%
    group_by(region, age_group, week_end) %>%
    summarise(all_cause=sum(all_cause))
  
  age1.region$source  <- 'NCHS'
  return(age1.region)
}