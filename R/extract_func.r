extract_func <- function(ds, measure=c('RR','excess','excess_inc'), round.level=0){
  
  measures <-  paste0(measure,'_', c('median','lcl','ucl'))
  
  med <- as.vector(as.matrix(as.data.frame(round(ds$preds.overall[,measures],round.level))))
  
  formatted.res <- paste0(med[1], ' (95%CI:', med[2],', ', med[3],')'  )
  return(formatted.res)
}