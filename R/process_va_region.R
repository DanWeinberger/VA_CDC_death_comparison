process_va_region <- function(ds){
 ds1 <- ds
  ds1$source <- 'VA'
  
  
  ds1 <- ds1[ds1$Age.Group %in% c('25-44 years' ,'45-64 years','65-74 years','75-84 years' ,  '85 years and older'),]
  
  ds1 <- ds1[,c('Week.Ending.Date', 'Age.Group','Number.of.Deaths', 'source', 'region')]
  
  names(ds1) <- c('week_end','age_group', 'all_cause', 'source','region')
  
  ds1$week_end <- as.Date(ds1$week_end)
  
  ds1 <- ds1[!is.na(ds1$all_cause),]
  
  ds1$all_cause <- as.numeric(as.character(ds1$all_cause))
  
  ds1$all_cause[is.na(ds1$all_cause)] <- 5
  
  #some values split across multiple rows around Jan 1
  ds2 <- ds1 %>%
    group_by(region, week_end,age_group,source) %>%
    summarize(all_cause = sum(all_cause))
  
  ds2$week_end <- ds2$week_end - 1

return(ds2)
}