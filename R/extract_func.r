extract_func <- function(ds, measure=c('RR','excess','excess_inc'), round.level=0){
  
  measures <-  paste0(measure,'_', c('median','lcl','ucl'))
  
  med <- as.vector(as.matrix(as.data.frame(round(ds$preds.overall[,measures],round.level))))
  
  formatted.res <- paste0(med[1], ' (95%CI:', med[2],', ', med[3],')'  )
  
  ds$preds.age <- ds$preds.age[ds$preds.age$date>='2020-01-01', c('agec','date',measures)]
  
 # formatted.age.qtr <- paste0(round(ds$preds.age[3], round.level), ' (95%CI:', round(ds$preds.age[4],round.level),', ', round(ds$preds.age[5], round.level),')'  )
  
  return(list('overall.results'=formatted.res, 'age.qtr.results'=ds$preds.age))
}
