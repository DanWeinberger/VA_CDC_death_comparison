plot_ts_region <- function( source_data='NCHS'){
  
  p1 <- ggplot(c1[c1$source==source_data,], aes(x=week_end, y=all_cause, group=region, col=region)) +
    geom_line() +
    ylab("Number of all-cause deaths") +
    xlab("Date") +
    theme_classic() +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
    geom_hline(yintercept=0, col='gray', lty=2) +
    facet_wrap(~age_group , scales='free', nrow=1) +
    theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) 
  return(p1)
}